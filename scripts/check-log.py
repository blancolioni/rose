#!/usr/bin/env python3
"""Check the checkpoint log on a Rose disk image.

Finds the log partition in the image's GPT, reads the log header, and
walks the entries the way the log server's startup scan does.  Each
entry should be a checkpoint: the kernel's tables, the storage
server's region table, then the memory server's tables and pages.
The check is that each part's header agrees with what follows it, that
the memory server's processes are the kernel's, that their segments'
regions are in the region table, and that the memory server's header
pages and the pages they list account for the rest of the entry.

After walking the entries it also looks at the location after the last
one, and reports anything there that looks like an entry header, such
as an entry left over from before a reset.

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

STORE_TABLE_MAGIC = 0x53544F5245544231
STORE_TABLE_VERSION = 2
REGIONS_PER_PAGE = PAGE_SIZE // 32
FRESH_BITS_PER_PAGE = PAGE_SIZE * 8

MEM_TABLE_MAGIC = 0x4D454D5441424C31
MEM_TABLE_VERSION = 1
MEM_PROCESS_MAGIC = 0x4D50524F43455331
MAX_SAVED_SEGMENTS = 8

KERNEL_TABLE_MAGIC = 0x4B5441424C455331
KERNEL_TABLE_VERSION = 3
MAX_SAVED_INTERRUPTS = 32
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

    def page(self, location):
        if location >= self.page_count:
            return None
        self.image.seek(self.base + location * PAGE_SIZE)
        return self.image.read(PAGE_SIZE)


def log_generation(header):
    return u64(header, 8)


def entry_fields(page):
    """Return (version, flags, generation, first, last) of an entry header,
    or None if the page is not one."""
    if page is None or u64(page, 0) != ENTRY_MAGIC:
        return None
    version = u32(page, 8)
    flags = u32(page, 12)
    if version == 1:
        return version, flags, None, u64(page, 16), u64(page, 24)
    return version, flags, u64(page, 16), u64(page, 24), u64(page, 32)


def describe_page(page):
    return "an empty page" if not any(page) else "something else"


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
    if version not in (2, KERNEL_TABLE_VERSION):
        return False, "kernel tables version %d" % version, 0, oids
    page_count = u32(page, 12)
    process_count = u32(page, 16)
    if first + page_count - 1 > last:
        return False, ("kernel tables of %d pages run past the entry"
                       % page_count), 0, oids

    location = first + 1
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

    # Version 3 has the next object id before the interrupts.  It must be
    # a process's object id, past every saved process's.
    next_oid = None
    interrupts_offset = 24
    if version >= 3:
        next_oid = u64(page, 24)
        interrupts_offset = 32
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

    return True, ("kernel: %d process%s in %d pages, %d interrupt%s%s"
                  % (process_count, "" if process_count == 1 else "es",
                     page_count, interrupt_count,
                     "" if interrupt_count == 1 else "s",
                     "" if next_oid is None
                     else ", next object id %d" % next_oid)), \
        page_count, oids


def check_store_tables(log, first, last, verbose):
    """Check the storage server's region table, which starts at page
    first.

    Returns (ok, description, page count, set of (base, bound))."""
    regions = set()
    page = log.page(first)
    if page is None or first > last or u64(page, 0) != STORE_TABLE_MAGIC:
        return False, ("location %d: expected the storage server's tables, "
                       "found %s" % (first, describe_page(page or b""))), \
            0, regions
    version = u32(page, 8)
    if version not in (1, STORE_TABLE_VERSION):
        return False, "storage tables version %d" % version, 0, regions
    page_count = u32(page, 12)
    region_count = u32(page, 16)
    fresh_pages = u32(page, 20) if version >= 2 else 0
    region_pages = (region_count + REGIONS_PER_PAGE - 1) // REGIONS_PER_PAGE
    if page_count != 1 + region_pages + fresh_pages \
            or first + page_count - 1 > last:
        return False, ("storage tables: %d pages for %d regions and %d "
                       "fresh pages"
                       % (page_count, region_count, fresh_pages)), 0, regions

    indices = set()
    space_pages = 0
    for i in range(region_count):
        record = log.page(first + 1 + i // REGIONS_PER_PAGE)
        index, device, base, bound, alloc_index, alloc_banks = \
            struct.unpack_from("<IIQQII", record,
                               (i % REGIONS_PER_PAGE) * 32)
        if index == 0 or index in indices or bound < base or device == 0:
            return False, ("storage region %d of %d: index %d, device %d, "
                           "pages %x..%x"
                           % (i + 1, region_count, index, device, base,
                              bound)), 0, regions
        indices.add(index)
        regions.add((base, bound))
        if alloc_index:
            space_pages += bound - base
        if verbose:
            print("    region %3d: %x..%x%s"
                  % (index, base, bound,
                     " space %d+%d" % (alloc_index, alloc_banks)
                     if alloc_index else " subregion"))

    fresh = ""
    if version >= 2:
        if fresh_pages != (space_pages + FRESH_BITS_PER_PAGE - 1) \
                // FRESH_BITS_PER_PAGE:
            return False, ("storage tables: %d fresh pages for %d pages "
                           "of regions" % (fresh_pages, space_pages)), \
                0, regions
        count = 0
        for i in range(fresh_pages):
            bits = log.page(first + 1 + region_pages + i)
            count += sum(bin(b).count("1") for b in bits)
        fresh = ", %d of %d pages fresh" % (count, space_pages)

    return True, ("storage: %d region%s%s"
                  % (region_count, "" if region_count == 1 else "s",
                     fresh)), \
        page_count, regions


def check_mem_tables(log, first, last, regions, verbose):
    """Check the memory server's tables, which start at page first.  The
    regions of their segments must be among regions.

    Returns (ok, description, page count, {mem process id: object id})."""
    processes = {}
    page = log.page(first)
    if page is None or first > last or u64(page, 0) != MEM_TABLE_MAGIC:
        return False, ("location %d: expected the memory server's tables, "
                       "found %s" % (first, describe_page(page or b""))), \
            0, processes
    version = u32(page, 8)
    if version != MEM_TABLE_VERSION:
        return False, "memory tables version %d" % version, 0, processes
    page_count = u32(page, 12)
    process_count = u32(page, 16)
    if page_count != process_count + 1 or first + page_count - 1 > last:
        return False, ("memory tables: %d pages for %d processes"
                       % (page_count, process_count)), 0, processes

    for i in range(process_count):
        location = first + 1 + i
        record = log.page(location)
        if u64(record, 0) != MEM_PROCESS_MAGIC:
            return False, ("location %d: expected memory process record %d "
                           "of %d" % (location, i + 1, process_count)), \
                0, processes
        oid = u64(record, 8)
        mem_pid, state, segment_count = struct.unpack_from("<III", record,
                                                           16)
        if segment_count > MAX_SAVED_SEGMENTS:
            return False, ("memory process %d: %d segments"
                           % (mem_pid, segment_count)), 0, processes
        processes[mem_pid] = oid
        if verbose:
            print("    memory process %d: object %d, %s, %d segments"
                  % (mem_pid, oid, "faulted" if state == 2 else "active",
                     segment_count))
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
                         + ("P" if flags & 32 else "-"),
                         "%x..%x +%d" % (r_base, r_bound, r_offset)
                         if r_base or r_bound else "none"))

    return True, ("memory tables: %d process%s"
                  % (process_count, "" if process_count == 1 else "es")), \
        page_count, processes


def check_entry(log, first, last, verbose):
    """Check the checkpoint stored in pages first .. last.

    Returns (ok, description)."""
    ok, kernel, kernel_pages, oids = check_kernel_tables(log, first, last,
                                                         verbose)
    if not ok:
        return False, kernel
    ok, store, store_pages, regions = check_store_tables(
        log, first + kernel_pages, last, verbose)
    if not ok:
        return False, kernel + "; " + store
    kernel = kernel + "; " + store
    ok, tables, table_pages, processes = check_mem_tables(
        log, first + kernel_pages + store_pages, last, regions, verbose)
    if not ok:
        return False, kernel + "; " + tables
    if set(processes.values()) != oids:
        return False, ("%s; %s: the memory server's processes %s are not "
                       "the kernel's %s"
                       % (kernel, tables, sorted(processes.values()),
                          sorted(oids)))
    ok, memory = check_memory(log,
                              first + kernel_pages + store_pages
                              + table_pages,
                              last, processes, verbose)
    return ok, kernel + "; " + tables + "; " + memory


def check_memory(log, first, last, processes, verbose):
    """Check a memory checkpoint stored in pages first .. last, whose pages
    belong to the memory server's processes in processes.

    Returns (ok, description)."""
    header_pages = 0
    listed = 0
    location = first
    while True:
        page = log.page(location)
        if page is None or location > last:
            return False, ("checkpoint header pages run past the entry "
                           "at location %d" % location)
        if u64(page, 0) != CHECKPOINT_MAGIC:
            return False, ("location %d: expected a checkpoint header "
                           "page, found %s"
                           % (location, describe_page(page)))
        header_pages += 1
        count = u32(page, 12)
        listed += count
        if verbose:
            print("    header page at %d lists %d pages"
                  % (location, count))
        for i in range(count):
            # A page reference is 40 bits: the memory server's process id
            # in the low 16, then the virtual page address.
            reference = int.from_bytes(page[16 + i * 5:21 + i * 5],
                                       "little")
            if reference & 0xFFFF not in processes:
                return False, ("location %d: page %05x belongs to memory "
                               "process %d, which is not in the tables"
                               % (location, reference >> 16,
                                  reference & 0xFFFF))
        if u32(page, 8) & IS_LAST_HEADER_PAGE:
            break
        location += 1

    expected = header_pages + listed
    actual = last - first + 1
    description = ("memory: %d header page%s, %d data pages"
                   % (header_pages, "" if header_pages == 1 else "s", listed))
    if expected != actual:
        return False, ("%s: expected %d pages, entry has %d"
                       % (description, expected, actual))
    return True, description


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
        print("log: blocks %d..%d, %d pages, generation %d"
              % (partition + (log.page_count, generation)))

        failures = 0
        entries = 0
        location = 1
        while True:
            fields = entry_fields(log.page(location))
            if fields is None:
                break
            version, flags, entry_generation, first, last = fields
            if version == 2 and entry_generation != generation:
                break
            if version not in (1, 2) or first != location + 1 \
               or last < location:
                print("location %d: entry header with version %d, pages "
                      "%d..%d does not fit; scan stops"
                      % (location, version, first, last))
                failures += 1
                break

            entries += 1
            ok, description = check_entry(log, first, last, args.verbose)
            print("entry %d at %d: version %d, pages %d..%d: %s%s"
                  % (entries, location, version, first, last,
                     description, "" if ok else "  ** BAD"))
            if not ok:
                failures += 1
            location = last + 1

        print("%d entr%s; next entry at location %d"
              % (entries, "y" if entries == 1 else "ies", location))

        fields = entry_fields(log.page(location))
        if fields is not None:
            version, flags, entry_generation, first, last = fields
            print("location %d holds an entry header that is not part of "
                  "this log (version %d, generation %s, pages %d..%d)"
                  % (location, version, entry_generation, first, last))

        if failures:
            print("%d inconsistent entr%s"
                  % (failures, "y" if failures == 1 else "ies"))
            return 1
        return 0


if __name__ == "__main__":
    sys.exit(main())
