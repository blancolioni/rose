#!/usr/bin/env python3
"""Check the checkpoint log on a Rose disk image.

Finds the log partition in the image's GPT, reads the log header, and
walks the entries the way the log server's startup scan does: from the
header's first entry, each entry with the next sequence number, wrapping
around from the end of the log to location 1 (#951).  Each
entry should be a checkpoint: the kernel's tables, the storage
server's region table, then the memory server's tables and pages.
The check is that each part's header agrees with what follows it, that
the memory server's processes include the kernel's, that their segments'
regions are in the region table, and that the memory server's header
pages, its shared header pages (ADR 0025) and the pages they list
account for the rest of the entry.

The memory server may have saved processes that the kernel did not: the
kernel leaves out a process that is still starting or has been killed
(#543, #703), and the memory server doesn't know about those states.
Restore leaves such a process out, with its pages, so the check allows
them, and names them in the entry's description (#723).

After walking the entries it also looks at the location after the last
one, and reports anything there that looks like an entry header, such
as an entry left over from before a reset.

The memory server cleans the log (#952): it copies the pages that are
still in use out of the oldest entries, then frees them, and new
entries can then overwrite them.  So in a cleaned log the first entries'
indexes can't be checked against earlier ones:

  - an entry whose index's own pages (directory and leaves) are not all
    in the log, from its start to the entry's end, can't be read at
    all, and is left unchecked;
  - the first entry whose index can be read is taken as it is, for the
    pages that its own batches don't hold;
  - each entry after that is checked as usual.

The last entry's index, which is the one a restore reads, has to be
readable, and has to name only pages that are in the log.

The layouts are those of src/servers/log/src/log-header.ads,
src/base/rose-checkpoints.ads and
src/library/kernelapi/devices/rose-devices-gpt.adb.

Usage: check-log.py [-v] IMAGE

With -v, it also lists the processes in each entry's kernel tables and
describes each memory checkpoint header page.

Exits with status 1 if the image has no valid log, or if any entry is
inconsistent.
"""

import argparse
import struct
import sys

BLOCK_SIZE = 512
PAGE_SIZE = 4096

GPT_MAGIC = 0x5452415020494645
LOG_TYPE = (0xE4BA963D64FDCF54, 0x4091DD328E257A2F)

LOG_MAGIC = 0x8F8FFFE0FA39FDF7
ENTRY_MAGIC = 0x7E753634A3568547
CHECKPOINT_MAGIC = 0x7E6AF17879CBE048
IS_LAST_HEADER_PAGE = 0x0001
MAX_PAGE_REFERENCES = (PAGE_SIZE - 16) // 5
INDEX_HEADER_MAGIC = 0x494E444558484431
MAX_INDEX_REFERENCES = (PAGE_SIZE - 16) // 4
INDEX_ENTRIES_PER_PAGE = 1024
DIRECTORY_SLOT = 0xFFFF
SHARED_HEADER_MAGIC = 0x5348415245445031
MAX_SHARED_REFERENCES = (PAGE_SIZE - 16) // 12
SHARED_PAGE_SAVED = 1
SHARED_PAGE_RELEASED = 2

STORE_TABLE_MAGIC = 0x53544F5245544231
STORE_TABLE_VERSION = 6
MAX_STORE_SLOTS = 255           # Rose.Checkpoints.Max_Store_Slots (#967)
RETIRED_GENERATION = 255
REGIONS_PER_PAGE = PAGE_SIZE // 32
BANKS_PER_PAGE = PAGE_SIZE // 24
MAX_TREE_HEIGHT = 7
MAX_STORE_PAGES = 2 ** 20

MEM_TABLE_MAGIC = 0x4D454D5441424C31
MEM_TABLE_VERSION = 2
MEM_HAS_SHARED_PAGES = 0x0001
MEM_HAS_INDEX = 0x0002
MEM_PROCESS_MAGIC = 0x4D50524F43455331
MAX_SAVED_SEGMENTS = 32

KERNEL_TABLE_MAGIC = 0x4B5441424C455331
KERNEL_TABLE_VERSION = 13
MAX_SAVED_INTERRUPTS = 32
RECORDS_PER_PAGE = PAGE_SIZE // 4
MAX_RECORD_PAGES = 4
MAX_RECORD_DEPTH = 8
RECORD_USED = 1
PROCESS_RECORD_MAGIC = 0x50524F4345535331
CAPS_PER_PAGE = PAGE_SIZE // 20

# Rose.Kernel.Processes.Process_State, in order.
PROCESS_STATES = ["available", "starting", "ready", "running", "blocked",
                  "interrupted", "faulted", "killed"]


def u32(data, offset):
    return struct.unpack_from("<I", data, offset)[0]


def u64(data, offset):
    return struct.unpack_from("<Q", data, offset)[0]


def find_log_partition(image):
    """Return (first block, last block) of the log partition, or None."""
    image.seek(BLOCK_SIZE)
    header = image.read(BLOCK_SIZE)
    if u64(header, 0) != GPT_MAGIC:
        return None
    # Rose.Devices.GPT always keeps the entries in block 2, and leaves
    # the header's partition entry LBA as zero.
    count = u32(header, 80)
    entry_size = u32(header, 84)
    image.seek(2 * BLOCK_SIZE)
    table = image.read(count * entry_size)
    for i in range(count):
        entry = table[i * entry_size:(i + 1) * entry_size]
        if (u64(entry, 0), u64(entry, 8)) == LOG_TYPE:
            return u64(entry, 32), u64(entry, 40)
    return None


class Log:

    def __init__(self, image, first_block, last_block):
        self.image = image
        self.base = first_block * BLOCK_SIZE
        self.page_count = ((last_block - first_block + 1) * BLOCK_SIZE
                           // PAGE_SIZE)

    # Locations 1 .. page_count - 1 hold entries, and wrap around from the
    # last back to 1 (#951).  The checks count an entry's pages on from
    # its header, so a location past the last is taken round to the start.

    def physical(self, location):
        """The log page that location names, after wrapping around."""
        if location < self.page_count:
            return location
        return (location - 1) % (self.page_count - 1) + 1

    def next(self, location):
        return 1 if location >= self.page_count - 1 else location + 1

    def distance(self, start, end):
        """How many locations end is after start, going round the log."""
        if end >= start:
            return end - start
        return self.page_count - 1 - start + end

    def page(self, location):
        self.image.seek(self.base + self.physical(location) * PAGE_SIZE)
        return self.image.read(PAGE_SIZE)


def log_generation(header):
    return u64(header, 8)


def log_start(header):
    """Return (first entry, its sequence number) from the log header.  A
    header from before version 3 has a sequence number of 0."""
    return u64(header, 4088), u64(header, 4080)


def entry_fields(page):
    """Return (version, flags, generation, first, last, sequence) of an
    entry header, or None if the page is not one.  Generation is None
    before version 2, and sequence before version 3."""
    if page is None or u64(page, 0) != ENTRY_MAGIC:
        return None
    version = u32(page, 8)
    flags = u32(page, 12)
    if version == 1:
        return version, flags, None, u64(page, 16), u64(page, 24), None
    return (version, flags, u64(page, 16), u64(page, 24), u64(page, 32),
            u64(page, 40) if version >= 3 else None)


def walk_entries(log, header):
    """Yield (location, first page, last page) of each entry of the log
    whose header is header, as the log server's startup scan finds them,
    for the scripts that change a log.  main does the same walk, and says
    why it stops."""
    generation = log_generation(header)
    start, sequence = log_start(header)
    if not 1 <= start < log.page_count:
        return
    location = start
    while True:
        fields = entry_fields(log.page(location))
        if fields is None:
            return
        version, flags, entry_generation, first, last, entry_sequence = \
            fields
        if version >= 2 and entry_generation != generation \
           or version >= 3 and entry_sequence != sequence \
           or version not in (1, 2, 3) \
           or first != log.next(location) \
           or not 1 <= last < log.page_count:
            return
        yield location, first, last
        location = log.next(last)
        if sequence:
            sequence += 1
        if location == start:
            return


def describe_page(page):
    return "an empty page" if not any(page) else "something else"


def check_records(log, first, count):
    """Check the derivation records, in the pages that start at first.

    Returns a description of what is wrong, or None.  A record that is used
    is one level below its parent, which is used, with at most eight levels,
    as the kernel checks when it restores them."""
    records = []
    for i in range(count):
        if i % RECORDS_PER_PAGE == 0:
            page = log.page(first + i // RECORDS_PER_PAGE)
        parent, flags = struct.unpack_from("<HH", page,
                                           (i % RECORDS_PER_PAGE) * 4)
        records.append((parent, flags))

    for number, (parent, flags) in enumerate(records, 1):
        if not flags & RECORD_USED:
            continue
        current, steps = number, 0
        while current:
            if current > count or not records[current - 1][1] & RECORD_USED \
               or steps == MAX_RECORD_DEPTH:
                return "derivation record %d is not %d levels or fewer " \
                    "below a root" % (number, MAX_RECORD_DEPTH)
            steps += 1
            current = records[current - 1][0]
    return None


def check_kernel_tables(log, first, last, verbose):
    """Check the kernel's tables at the start of an entry whose pages are
    first .. last.

    Returns (ok, description, page count, object ids)."""
    oids = set()
    page = log.page(first)
    if u64(page, 0) != KERNEL_TABLE_MAGIC:
        return False, ("location %d: expected the kernel's tables, found %s"
                       % (first, describe_page(page))), 0, oids
    version = u32(page, 8)
    if not 2 <= version <= KERNEL_TABLE_VERSION:
        return False, "kernel tables version %d" % version, 0, oids
    page_count = u32(page, 12)
    process_count = u32(page, 16)
    if first + page_count - 1 > last:
        return False, ("kernel tables of %d pages run past the entry"
                       % page_count), 0, oids

    # Version 6 has the derivation records after the header page.
    record_pages = 0
    record_count = 0
    if version >= 6:
        record_pages = u32(page, 32)
        record_count = u32(page, 36)
        if record_pages > MAX_RECORD_PAGES \
           or record_pages != (record_count + RECORDS_PER_PAGE - 1) \
           // RECORDS_PER_PAGE:
            return False, ("kernel tables: %d derivation records do not "
                           "fit %d pages" % (record_count, record_pages)), \
                0, oids
        problem = check_records(log, first + 1, record_count)
        if problem:
            return False, "kernel tables: " + problem, 0, oids

    location = first + 1 + record_pages
    for i in range(process_count):
        record = log.page(location)
        if location > first + page_count - 1 \
           or u64(record, 0) != PROCESS_RECORD_MAGIC:
            return False, ("location %d: expected process record %d of %d"
                           % (location, i + 1, process_count)), 0, oids
        oid = u64(record, 8)
        oids.add(oid)
        name = record[16:32].decode("latin-1").rstrip(" \0")
        state = u32(record, 32)
        last_cap, cap_count, cap_pages = struct.unpack_from("<III",
                                                            record, 56)
        if cap_pages != (cap_count + CAPS_PER_PAGE - 1) // CAPS_PER_PAGE \
           or cap_count > last_cap:
            return False, ("process %d (%s): %d caps up to %d do not fit "
                           "%d cap pages"
                           % (oid, name, cap_count, last_cap,
                              cap_pages)), 0, oids
        if verbose:
            print("    process %d %-16s %-11s %3d caps"
                  % (oid, name or "(no name)",
                     PROCESS_STATES[state] if state < len(PROCESS_STATES)
                     else "state %d" % state, cap_count))
        location += 1 + cap_pages

    if location != first + page_count:
        return False, ("kernel tables hold %d pages of records, header "
                       "says %d" % (location - first, page_count)), 0, oids

    interrupt_count = u32(page, 20)
    if interrupt_count > MAX_SAVED_INTERRUPTS:
        return False, "kernel tables: %d interrupts" % interrupt_count, \
            0, oids

    # Version 3 has the next object id before the interrupts, and version 4
    # the FPU state in each process record, which isn't checked.  It must be
    # a process's object id, past every saved process's.
    next_oid = None
    interrupts_offset = 24
    if version >= 3:
        next_oid = u64(page, 24)
        interrupts_offset = 32 if version < 6 else 40
        if not 0 < next_oid <= 0xFFFF_FFFF \
           or any(oid >= next_oid for oid in oids):
            return False, ("kernel tables: next object id %d is not past "
                           "every process's" % next_oid), 0, oids

    for i in range(interrupt_count):
        vector, cap, oid = struct.unpack_from("<IIQ", page,
                                              interrupts_offset + i * 16)
        if oid not in oids:
            return False, ("interrupt %d goes to process %d, which is not "
                           "in the kernel tables" % (vector, oid)), 0, oids
        if verbose:
            print("    interrupt %d -> process %d cap %d" % (vector, oid, cap))

    # Version 9 has the interfaces of the boot set that wrote it after the
    # interrupts (#375).
    interfaces = ""
    if version >= 9:
        major, minor, level, digest = struct.unpack_from(
            "<HHIQ", page, 40 + MAX_SAVED_INTERRUPTS * 16)
        interfaces = ", interfaces %d.%d level %d" % (major, minor, level)

    return True, ("kernel: %d process%s in %d pages, %d interrupt%s%s%s%s"
                  % (process_count, "" if process_count == 1 else "es",
                     page_count, interrupt_count,
                     "" if interrupt_count == 1 else "s",
                     "" if next_oid is None
                     else ", next object id %d" % next_oid,
                     "" if not record_count
                     else ", %d derivation records" % record_count,
                     interfaces)), \
        page_count, oids


def check_store_tables(log, first, last, verbose):
    """Check the storage server's region table and banks, which start at
    page first.  The block map is a tree on the swap partition (ADR 0038),
    which this doesn't read: only its root, height and image copy, in the
    header.

    Returns (ok, description, page count, set of (base, bound))."""
    regions = set()
    page = log.page(first)
    if page is None or first > last or u64(page, 0) != STORE_TABLE_MAGIC:
        return False, ("location %d: expected the storage server's tables, "
                       "found %s" % (first, describe_page(page or b""))), \
            0, regions
    next_page = u64(page, 8)
    version, page_count, region_count, tree_height, image_copy, \
        bank_count, bank_pages, _ = struct.unpack_from("<IIIIIIII", page, 16)
    tree_root = u64(page, 48)
    if version != STORE_TABLE_VERSION:
        return False, "storage tables version %d" % version, 0, regions
    region_pages = (region_count + REGIONS_PER_PAGE - 1) // REGIONS_PER_PAGE
    if not 1 <= tree_height <= MAX_TREE_HEIGHT \
            or image_copy > 1 \
            or not 0 < tree_root < MAX_STORE_PAGES:
        return False, ("storage tables: block map root %d, height %d, "
                       "image copy %d" % (tree_root, tree_height,
                                          image_copy)), 0, regions
    if bank_count == 0 \
            or bank_pages != (bank_count + BANKS_PER_PAGE - 1) \
            // BANKS_PER_PAGE \
            or page_count != 1 + region_pages + bank_pages \
            or first + page_count - 1 > last:
        return False, ("storage tables: %d pages for %d regions and %d "
                       "banks (%d pages)"
                       % (page_count, region_count, bank_count,
                          bank_pages)), 0, regions

    # Each slot's generation (#967), which no region or bank in the
    # tables can have retired.
    region_generations = page[56:56 + MAX_STORE_SLOTS]
    bank_generations = page[56 + MAX_STORE_SLOTS:56 + 2 * MAX_STORE_SLOTS]
    retired = sum(1 for g in region_generations + bank_generations
                  if g == RETIRED_GENERATION)

    # The banks come last (#518).  Each one's parent must be another bank
    # in the table, and the root bank, 1, has none.
    banks = {}
    for i in range(bank_count):
        record = log.page(first + 1 + region_pages
                          + i // BANKS_PER_PAGE)
        index, parent, page_limit, region_limit, _ = struct.unpack_from(
            "<IIQII", record, (i % BANKS_PER_PAGE) * 24)
        if index == 0 or index in banks or index > MAX_STORE_SLOTS:
            return False, ("storage bank %d of %d: index %d"
                           % (i + 1, bank_count, index)), 0, regions
        if bank_generations[index - 1] == RETIRED_GENERATION:
            return False, ("storage bank %d is in a retired slot"
                           % index), 0, regions
        banks[index] = parent
        if verbose:
            print("    bank %3d: parent %d, limits %d pages, %d regions"
                  % (index, parent, page_limit, region_limit))
    if banks.get(1, None) != 0:
        return False, "storage banks: no root bank", 0, regions
    for index, parent in banks.items():
        if index != 1 and (parent not in banks or parent == index):
            return False, ("storage bank %d has parent %d, which is not in "
                           "the tables" % (index, parent)), 0, regions

    indices = set()
    roots = {}
    mapped = 0
    for i in range(region_count):
        record = log.page(first + 1 + i // REGIONS_PER_PAGE)
        index, root, base, bound, pages, bank = \
            struct.unpack_from("<IIQQII", record,
                               (i % REGIONS_PER_PAGE) * 32)
        if index == 0 or index in indices or index > MAX_STORE_SLOTS \
                or bound < base or bound > next_page:
            return False, ("storage region %d of %d: index %d, pages "
                           "%x..%x, next page %x"
                           % (i + 1, region_count, index, base, bound,
                              next_page)), 0, regions
        if region_generations[index - 1] == RETIRED_GENERATION:
            return False, ("storage region %d is in a retired slot"
                           % index), 0, regions
        if bank not in banks:
            return False, ("storage region %d is charged to bank %d, which "
                           "is not in the tables" % (index, bank)), \
                0, regions
        if pages > bound - base or (root != index and pages != 0):
            return False, ("storage region %d: %d pages with space, of "
                           "%d" % (index, pages, bound - base)), 0, regions
        indices.add(index)
        roots[index] = root
        regions.add((base, bound))
        mapped += pages
        if verbose:
            print("    region %3d: %x..%x, %d pages with space, bank %d%s"
                  % (index, base, bound, pages, bank,
                     "" if root == index else " subregion of %d" % root))
    for index, root in roots.items():
        if root not in indices:
            return False, ("storage region %d is a subregion of region %d, "
                           "which is not in the tables" % (index, root)), \
                0, regions

    return True, ("storage: %d region%s, %d page%s with space, block map "
                  "height %d, %d bank%s%s"
                  % (region_count, "" if region_count == 1 else "s",
                     mapped, "" if mapped == 1 else "s", tree_height,
                     bank_count, "" if bank_count == 1 else "s",
                     ", %d retired slot%s" % (retired,
                                              "" if retired == 1 else "s")
                     if retired else "")), \
        page_count, regions


def check_mem_tables(log, first, last, regions, verbose, info):
    """Check the memory server's tables, which start at page first.  The
    regions of their segments must be among regions.  Sets info's
    "has_index" and "directories" ({mem process id: log page of its index
    directory}).

    Returns (ok, description, page count, {mem process id: object id},
    whether the pages of shared segments follow)."""
    processes = {}
    page = log.page(first)
    if page is None or first > last or u64(page, 0) != MEM_TABLE_MAGIC:
        return False, ("location %d: expected the memory server's tables, "
                       "found %s" % (first, describe_page(page or b""))), \
            0, processes, False
    version = u32(page, 8)
    if version != MEM_TABLE_VERSION:
        return False, "memory tables version %d" % version, 0, processes, \
            False
    page_count = u32(page, 12)
    process_count = u32(page, 16)
    has_shared = bool(u32(page, 20) & MEM_HAS_SHARED_PAGES)
    info["has_index"] = bool(u32(page, 20) & MEM_HAS_INDEX)
    info["directories"] = {}
    if page_count != process_count + 1 or first + page_count - 1 > last:
        return False, ("memory tables: %d pages for %d processes"
                       % (page_count, process_count)), 0, processes, False

    for i in range(process_count):
        location = first + 1 + i
        record = log.page(location)
        if u64(record, 0) != MEM_PROCESS_MAGIC:
            return False, ("location %d: expected memory process record %d "
                           "of %d" % (location, i + 1, process_count)), \
                0, processes, False
        oid = u64(record, 8)
        mem_pid, state, segment_count, directory = \
            struct.unpack_from("<IIII", record, 16)
        if segment_count > MAX_SAVED_SEGMENTS:
            return False, ("memory process %d: %d segments"
                           % (mem_pid, segment_count)), 0, processes, False
        if directory >= log.page_count:
            return False, ("memory process %d: index directory at log page "
                           "%d, past the log" % (mem_pid, directory)), \
                0, processes, False
        processes[mem_pid] = oid
        info["directories"][mem_pid] = directory
        if verbose:
            print("    memory process %d: object %d, %s, %d segments%s"
                  % (mem_pid, oid, "faulted" if state == 2 else "active",
                     segment_count,
                     ", index directory at log page %d" % directory
                     if directory else ""))
        for s in range(segment_count):
            base, bound, limit, flags, r_base, r_bound, r_offset = \
                struct.unpack_from("<IIIIQQQ", record, 32 + s * 40)
            if (r_base or r_bound) and (r_base, r_bound) not in regions:
                return False, ("memory process %d: segment %05x..%05x uses "
                               "region %x..%x, which the storage server "
                               "doesn't have"
                               % (mem_pid, base, bound, r_base, r_bound)), \
                    0, processes
            if verbose:
                print("      %05x..%05x %s region %s"
                      % (base, bound,
                         "".join(c if flags & bit else "-" for c, bit in
                                 (("r", 1), ("w", 2), ("x", 4), ("e", 8),
                                  ("p", 16)))
                         + ("P" if flags & 32 else "-")
                         + ("S" if flags & 64 else "-"),
                         "%x..%x +%d" % (r_base, r_bound, r_offset)
                         if r_base or r_bound else "none"))

    return True, ("memory tables: %d process%s"
                  % (process_count, "" if process_count == 1 else "es")), \
        page_count, processes, has_shared


def check_entry(log, first, last, verbose, info):
    """Check the checkpoint stored in pages first .. last.  Sets info's
    "processes" ({mem process id: object id}), "batches" ([(first page,
    header pages)], in order) and what check_mem_tables sets, for
    check_index_contents.

    Returns (ok, description)."""
    # Batches paged out between checkpoints come before the kernel's
    # tables (ADR 0028, section 4).
    ok, message, leading, kernel_first = find_sections(
        log, first, last, KERNEL_TABLE_MAGIC, "the kernel's tables")
    if not ok:
        return False, message
    for kind, start, headers, listed in leading:
        if kind != "batch":
            return False, ("location %d: an index section before the "
                           "kernel's tables" % start)

    ok, kernel, kernel_pages, oids = check_kernel_tables(
        log, kernel_first, last, verbose)
    if not ok:
        return False, kernel
    ok, store, store_pages, regions = check_store_tables(
        log, kernel_first + kernel_pages, last, verbose)
    if not ok:
        return False, kernel + "; " + store
    kernel = kernel + "; " + store

    # The memory server's tables come after its batches and index sections
    # (ADR 0028), so they are found by skipping those.
    ok, message, sections, mem_first = find_sections(
        log, kernel_first + kernel_pages + store_pages, last,
        MEM_TABLE_MAGIC, "the memory server's tables")
    if not ok:
        return False, kernel + "; " + message
    sections = leading + sections

    ok, tables, table_pages, processes, has_shared = check_mem_tables(
        log, mem_first, last, regions, verbose, info)
    info["processes"] = processes
    info["batches"] = [(start, headers)
                       for kind, start, headers, listed in sections
                       if kind == "batch"]
    info["leading_batches"] = sum(1 for kind, start, headers, listed
                                  in leading if kind == "batch")
    if not ok:
        return False, kernel + "; " + tables
    missing = oids - set(processes.values())
    if missing:
        return False, ("%s; %s: the kernel's processes %s are not in the "
                       "memory server's %s"
                       % (kernel, tables, sorted(missing),
                          sorted(processes.values())))
    unsaved = sorted(set(processes.values()) - oids)
    if unsaved:
        tables += ("; %d process%s not saved by the kernel: %s"
                   % (len(unsaved), "" if len(unsaved) == 1 else "es",
                      ", ".join(str(oid) for oid in unsaved)))
    description = kernel + "; " + tables

    batches = 0
    batch_headers = 0
    batch_pages = 0
    wrapped_batches = 0
    gone_pages = 0
    indexes = 0
    index_pages = 0
    for number, (kind, start, headers, listed) in enumerate(sections):
        if kind == "batch":
            # A batch before the kernel's tables was paged out between
            # checkpoints, maybe before a checkpoint that failed (#971),
            # and may hold pages of processes gone by this checkpoint.
            ok, message, gone = check_batch(log, start, headers, processes,
                                            verbose,
                                            number < len(leading))
            batches += 1
            batch_headers += headers
            batch_pages += listed
            gone_pages += gone
            # Locations here count on past the end of the log, so a batch
            # whose pages wrap around starts before it and ends after it
            # (#989).
            if start + headers < log.page_count \
               <= start + headers + listed - 1:
                wrapped_batches += 1
        else:
            ok, message = check_index(log, start, headers, processes,
                                      verbose)
            indexes += 1
            index_pages += listed
        if not ok:
            return False, description + "; " + message
    if batches == 0:
        return False, description + "; no batch of memory pages"

    description += ("; memory: %d batch%s, %d header page%s, %d data pages"
                    % (batches, "" if batches == 1 else "es",
                       batch_headers, "" if batch_headers == 1 else "s",
                       batch_pages))
    if wrapped_batches:
        description += "; a batch wraps round the log"
    if leading:
        description += ("; %d paged out before the checkpoint"
                        % sum(1 for kind, start, headers, listed in leading
                              if kind == "batch"))
    if gone_pages:
        description += ("; %d page%s of processes that are gone"
                        % (gone_pages, "" if gone_pages == 1 else "s"))
    if indexes:
        description += ("; index: %d section%s, %d page%s"
                        % (indexes, "" if indexes == 1 else "s",
                           index_pages, "" if index_pages == 1 else "s"))

    end = mem_first + table_pages
    if has_shared:
        ok, shared, shared_headers, saved = check_shared(log, end, last,
                                                         verbose)
        if not ok:
            return False, description + "; " + shared
        description += "; " + shared
        end += shared_headers + saved

    if end != last + 1:
        return False, ("%s: expected %d pages, entry has %d"
                       % (description, end - first, last - first + 1))
    return True, description


def find_sections(log, first, last, stop_magic, stop_name):
    """Find the batches and index sections that start at page first, up to
    the page whose magic number is stop_magic, which starts stop_name.
    Only their header pages are read.

    Returns (ok, message, [(kind, first page, header pages, pages listed)],
    the page with stop_magic)."""
    sections = []
    location = first
    while True:
        page = log.page(location)
        if page is None or location > last:
            return False, ("expected %s after the batches, at location %d"
                           % (stop_name, location)), sections, location
        magic = u64(page, 0)
        if magic == stop_magic:
            return True, None, sections, location
        if magic == CHECKPOINT_MAGIC:
            kind, limit = "batch", MAX_PAGE_REFERENCES
        elif magic == INDEX_HEADER_MAGIC:
            kind, limit = "index", MAX_INDEX_REFERENCES
        else:
            return False, ("location %d: expected a checkpoint or index "
                           "header page, found %s"
                           % (location, describe_page(page))), \
                sections, location

        start = location
        headers = 0
        listed = 0
        while True:
            page = log.page(location)
            if page is None or location > last:
                return False, ("%s header pages run past the entry at "
                               "location %d" % (kind, location)), \
                    sections, location
            if u64(page, 0) != magic:
                return False, ("location %d: expected another %s header "
                               "page, found %s"
                               % (location, kind, describe_page(page))), \
                    sections, location
            count = u32(page, 12)
            if count > limit:
                return False, ("location %d: %s header page lists %d pages"
                               % (location, kind, count)), \
                    sections, location
            headers += 1
            listed += count
            if u32(page, 8) & IS_LAST_HEADER_PAGE:
                break
            location += 1

        if start + headers + listed - 1 > last:
            return False, ("location %d: %s lists %d pages, past the entry"
                           % (start, kind, listed)), sections, start
        sections.append((kind, start, headers, listed))
        location = start + headers + listed


def check_batch(log, first, headers, processes, verbose, paged_out):
    """Check the header pages of the batch that starts at page first: each
    page they list belongs to one of the memory server's processes.  If
    paged_out, the batch was paged out between checkpoints, and its pages
    may belong to processes that are gone (ADR 0028, section 4).

    Returns (ok, message, pages of processes that are gone)."""
    gone = 0
    for location in range(first, first + headers):
        page = log.page(location)
        count = u32(page, 12)
        if verbose:
            print("    header page at %d lists %d pages" % (location, count))
        for i in range(count):
            # A page reference is 40 bits: the memory server's process id
            # in the low 16, then the virtual page address.
            reference = int.from_bytes(page[16 + i * 5:21 + i * 5],
                                       "little")
            if reference & 0xFFFF not in processes:
                if paged_out:
                    gone += 1
                    continue
                return False, ("location %d: page %05x belongs to memory "
                               "process %d, which is not in the tables"
                               % (location, reference >> 16,
                                  reference & 0xFFFF)), 0
    return True, None, gone


def check_index(log, first, headers, processes, verbose):
    """Check the index section that starts at page first (ADR 0028): each
    index page it lists belongs to one of the memory server's processes,
    and every log page it names is in the log.

    Returns (ok, message)."""
    location = first + headers
    for header in range(first, first + headers):
        page = log.page(header)
        count = u32(page, 12)
        if verbose:
            print("    index header page at %d lists %d pages"
                  % (header, count))
        for i in range(count):
            process, slot = struct.unpack_from("<HH", page, 16 + i * 4)
            if process not in processes:
                return False, ("location %d: index page for memory process "
                               "%d, which is not in the tables"
                               % (header, process))
            if slot != DIRECTORY_SLOT and slot >= INDEX_ENTRIES_PER_PAGE:
                return False, ("location %d: index page for slot %d"
                               % (header, slot))
            index = log.page(location)
            for entry in struct.unpack_from("<%dI" % INDEX_ENTRIES_PER_PAGE,
                                            index):
                if entry >= log.page_count:
                    return False, ("location %d: index page names log page "
                                   "%d, past the log" % (location, entry))
            if verbose:
                print("      %s of memory process %d at %d"
                      % ("directory" if slot == DIRECTORY_SLOT
                         else "leaf %d" % slot, process, location))
            location += 1
    return True, None


def read_index(log, directory):
    """Return {virtual page: log page} from the index whose directory is
    at log page directory, or {} if directory is 0."""
    pages = {}
    if not directory:
        return pages
    leaves = struct.unpack_from("<%dI" % INDEX_ENTRIES_PER_PAGE,
                                log.page(directory))
    for slot, leaf in enumerate(leaves):
        if not leaf:
            continue
        entries = struct.unpack_from("<%dI" % INDEX_ENTRIES_PER_PAGE,
                                     log.page(leaf))
        for i, log_page in enumerate(entries):
            if log_page:
                pages[slot * INDEX_ENTRIES_PER_PAGE + i] = log_page
    return pages


def index_pages(log, directory):
    """The log pages of the index whose directory is at log page
    directory: the directory and its leaves."""
    if not directory:
        return []
    leaves = struct.unpack_from("<%dI" % INDEX_ENTRIES_PER_PAGE,
                                log.page(directory))
    return [directory] + [leaf for leaf in leaves if leaf]


def batch_pages(log, first, headers):
    """Yield (mem process id, virtual page, log page) for each page of the
    batch whose header pages start at page first."""
    location = first + headers
    for header in range(first, first + headers):
        page = log.page(header)
        for i in range(u32(page, 12)):
            # 16 bits of process id, then 20 of virtual page; the last
            # four bits are padding, which mem doesn't clear.
            reference = int.from_bytes(page[16 + i * 5:21 + i * 5],
                                       "little")
            yield (reference & 0xFFFF, (reference >> 16) & 0xFFFFF,
                   log.physical(location))
            location += 1


def check_index_contents(log, info, previous, baseline=False):
    """Check that the index of each process in the entry that info
    describes names the latest copy of each of its pages: the copies its
    index in the previous entry named, updated by the batches of this
    entry.  previous is the info of the previous entry, or None.

    A process's index carries on from the previous entry if it was there
    with the same memory process id, and its object id was there too: a
    process that took over another's object id with a replacement (ADR
    0032) keeps its slot and its pages.  A process that is new to its
    slot starts with nothing.  Each process's index is read into info's
    "indexes", for the next entry.

    baseline is True for the first readable index of a cleaned log
    (#952): the entries before it were freed, or their indexes can't be
    read, so each process starts with the pages of its index that its
    batches don't hold, which can't be checked.  They are counted in
    info's "baseline_pages".

    A slot can have a new process between a batch paged out before the
    checkpoint and the checkpoint itself, and the memory server forgets
    the old process's pages then (#974).  So, for a process that is new
    to its slot, a page of a batch before the checkpoint that its index
    doesn't name is taken to be an earlier process's, and is counted in
    info's "earlier_pages".

    Returns a description of what is wrong, or None."""
    info["indexes"] = {}
    carried = previous is not None and previous.get("has_index")
    old_oids = set(previous["processes"].values()) if carried else set()
    expected = {}
    new_to_slot = set()
    for pid, oid in info["processes"].items():
        if carried and pid in previous["processes"] and oid in old_oids:
            expected[pid] = dict(previous["indexes"].get(pid, {}))
        else:
            expected[pid] = {}
            new_to_slot.add(pid)
    maybe_earlier = {pid: set() for pid in expected}
    leading = info.get("leading_batches", 0)
    for number, (first, headers) in enumerate(info["batches"]):
        for pid, virtual, location in batch_pages(log, first, headers):
            if pid in expected:
                expected[pid][virtual] = location
                if number < leading and pid in new_to_slot:
                    maybe_earlier[pid].add(virtual)
                else:
                    maybe_earlier[pid].discard(virtual)

    problems = []
    info["earlier_pages"] = 0
    info["baseline_pages"] = 0
    for pid in sorted(info["processes"]):
        actual = read_index(log, info["directories"].get(pid, 0))
        info["indexes"][pid] = actual
        if baseline:
            for virtual, location in actual.items():
                if virtual not in expected[pid]:
                    expected[pid][virtual] = location
                    info["baseline_pages"] += 1
        for virtual in maybe_earlier[pid]:
            if virtual not in actual:
                del expected[pid][virtual]
                info["earlier_pages"] += 1
        if actual == expected[pid]:
            continue
        wrong = sorted(set(actual) | set(expected[pid]),
                       key=lambda v: (actual.get(v) == expected[pid].get(v),
                                      v))
        virtual = wrong[0]
        count = sum(1 for v in wrong
                    if actual.get(v) != expected[pid].get(v))
        problems.append("memory process %d (object %d): %d page%s differ, "
                        "such as %05x: index has log page %s, the log %s"
                        % (pid, info["processes"][pid], count,
                           "" if count == 1 else "s", virtual,
                           actual.get(virtual), expected[pid].get(virtual)))
    if problems:
        return "index: " + "; ".join(problems)
    return None


def check_shared(log, first, last, verbose):
    """Check the shared header pages, which start at page first.

    Returns (ok, description, header pages, saved pages)."""
    header_pages = 0
    saved = 0
    released = 0
    location = first
    while True:
        page = log.page(location)
        if page is None or location > last:
            return False, ("shared header pages run past the entry at "
                           "location %d" % location), 0, 0
        if u64(page, 0) != SHARED_HEADER_MAGIC:
            return False, ("location %d: expected a shared header page, "
                           "found %s" % (location, describe_page(page))), \
                0, 0
        count = u32(page, 12)
        if count > MAX_SHARED_REFERENCES:
            return False, ("location %d: shared header page lists %d pages"
                           % (location, count)), 0, 0
        header_pages += 1
        if verbose:
            print("    shared header page at %d lists %d pages"
                  % (location, count))
        for i in range(count):
            store_page, flags = struct.unpack_from("<QI", page, 16 + i * 12)
            if flags == SHARED_PAGE_SAVED:
                saved += 1
            elif flags == SHARED_PAGE_RELEASED:
                released += 1
            else:
                return False, ("location %d: shared page %x has flags %d"
                               % (location, store_page, flags)), 0, 0
            if verbose:
                print("      shared page %x %s"
                      % (store_page, "saved" if flags == SHARED_PAGE_SAVED
                         else "released"))
        if u32(page, 8) & IS_LAST_HEADER_PAGE:
            break
        location += 1

    return True, ("shared: %d header page%s, %d saved, %d released"
                  % (header_pages, "" if header_pages == 1 else "s",
                     saved, released)), header_pages, saved


def main():
    parser = argparse.ArgumentParser(
        description="Check the checkpoint log on a Rose disk image.")
    parser.add_argument("image")
    parser.add_argument("-v", "--verbose", action="store_true",
                        help="list each entry's processes and memory "
                        "checkpoint header pages")
    args = parser.parse_args()

    with open(args.image, "rb") as image:
        partition = find_log_partition(image)
        if partition is None:
            print("%s: no GPT log partition" % args.image)
            return 1

        log = Log(image, *partition)
        header = log.page(0)
        if u64(header, 0) != LOG_MAGIC:
            print("log partition (blocks %d..%d) has no log header"
                  % partition)
            return 1

        generation = log_generation(header)
        start, sequence = log_start(header)
        print("log: blocks %d..%d, %d pages, generation %d"
              % (partition + (log.page_count, generation)))

        # A log from before version 3 starts at 1, and its sequence
        # number is 0.  From version 3, the log starts at the header's
        # first entry, and each entry has the next sequence number: one
        # with another is left over from an earlier lap (#951).
        if not 1 <= start < log.page_count:
            print("log header's first entry, %d, is outside the log" % start)
            return 1
        if sequence:
            print("log starts at location %d, sequence number %d"
                  % (start, sequence))

        failures = 0
        entries = 0
        indexed = 0
        wrapped = False
        location = start
        previous = None
        last_info = None
        # A log whose first entry is not the first one written has been
        # cleaned (#952); until an entry's index can be read, there is
        # nothing to check the indexes against.
        cleaned = sequence > 1
        baseline = cleaned
        while True:
            fields = entry_fields(log.page(location))
            if fields is None:
                break
            version, flags, entry_generation, first, last, \
                entry_sequence = fields
            if version >= 2 and entry_generation != generation:
                break
            if version >= 3 and entry_sequence != sequence:
                break
            if version not in (1, 2, 3) \
               or first != log.next(location) \
               or not 1 <= last < log.page_count \
               or (version < 3 and last < location):
                print("location %d: entry header with version %d, pages "
                      "%d..%d does not fit; scan stops"
                      % (location, version, first, last))
                failures += 1
                break

            # The checks count on from the header, past the end of the log
            # if the entry wraps around.
            end = location + log.distance(location, last)
            if end >= log.page_count or location < start:
                wrapped = True

            entries += 1
            info = {}
            ok, description = check_entry(log, location + 1, end,
                                          args.verbose, info)
            unread = 0
            if ok and info["has_index"] and baseline:
                # The pages of the log up to this entry's end are the
                # ones its index can be in; the rest were freed, and
                # may have been written over.
                unread = sum(
                    1 for directory in info["directories"].values()
                    for at in index_pages(log, directory)
                    if not 1 <= at < log.page_count
                    or log.distance(start, at) > log.distance(start, last))
            if unread:
                description += ("; index not checked: %d of its pages "
                                "are in freed entries" % unread)
            elif ok and info["has_index"]:
                # An entry's index has to agree with the log's pages
                # (ADR 0028): a restore that reads only the last entry
                # gets what one that reads every entry would.
                problem = check_index_contents(log, info, previous,
                                               baseline=baseline)
                baseline = False
                if problem:
                    ok, description = False, description + "; " + problem
                else:
                    indexed += 1
                if info.get("baseline_pages"):
                    description += ("; %d page%s of its index taken as "
                                    "they are" % (info["baseline_pages"],
                                                  "" if info["baseline_pages"]
                                                  == 1 else "s"))
                if info.get("earlier_pages"):
                    description += ("; %d page%s of earlier processes in "
                                    "reused slots"
                                    % (info["earlier_pages"],
                                       "" if info["earlier_pages"] == 1
                                       else "s"))
            print("entry %d at %d: version %d, %spages %d..%d: %s%s"
                  % (entries, location, version,
                     "" if entry_sequence is None
                     else "sequence %d, " % entry_sequence,
                     first, last, description, "" if ok else "  ** BAD"))
            if not ok:
                failures += 1
            previous = info if "indexes" in info else None
            last_info = info if ok else None
            location = log.next(last)
            if sequence:
                sequence += 1
            if location == start:
                print("the log is full")
                break

        print("%d entr%s, %d with an index that agrees with the log; "
              "next entry at location %d%s"
              % (entries, "y" if entries == 1 else "ies", indexed,
                 location, "; the log has wrapped around" if wrapped
                 else ""))

        # The last entry's index is what a restore reads: every page it
        # names has to be in the log, not in an entry that was freed.
        if cleaned and last_info is not None and last_info.get("has_index") \
           and "indexes" not in last_info:
            print("the last entry's index is in freed entries")
            failures += 1
        elif cleaned and last_info is not None and "indexes" in last_info:
            used = log.distance(start, location) if location != start \
                else log.page_count - 1
            outside = [(pid, virtual, at)
                       for pid, index in sorted(last_info["indexes"].items())
                       for virtual, at in sorted(index.items())
                       if at == 0 or log.distance(start, at) >= used]
            if outside:
                pid, virtual, at = outside[0]
                print("the last entry's index names %d page%s outside the "
                      "log, such as memory process %d page %05x at %d"
                      % (len(outside), "" if len(outside) == 1 else "s",
                         pid, virtual, at))
                failures += 1

        fields = entry_fields(log.page(location))
        if fields is not None and location != start:
            version, flags, entry_generation, first, last, \
                entry_sequence = fields
            print("location %d holds an entry header that is not part of "
                  "this log (version %d, generation %s, sequence %s, "
                  "pages %d..%d)"
                  % (location, version, entry_generation, entry_sequence,
                     first, last))

        if failures:
            print("%d inconsistent entr%s"
                  % (failures, "y" if failures == 1 else "ies"))
            return 1
        return 0


if __name__ == "__main__":
    sys.exit(main())
