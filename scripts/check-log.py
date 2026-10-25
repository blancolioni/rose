#!/usr/bin/env python3
"""Check the checkpoint log on a Rose disk image.

Finds the log partition in the image's GPT, reads the log header, and
walks the entries the way the log server's startup scan does.  Each
entry should be a checkpoint: the kernel's tables, then the memory
server's pages.  The check is that the kernel's header page, process
records and capability pages agree with each other, and that the
memory server's header pages and the pages they list account for the
rest of the entry.

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

KERNEL_TABLE_MAGIC = 0x4B5441424C455331
KERNEL_TABLE_VERSION = 1
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

    Returns (ok, description, page count)."""
    page = log.page(first)
    if u64(page, 0) != KERNEL_TABLE_MAGIC:
        return False, ("location %d: expected the kernel's tables, found %s"
                       % (first, describe_page(page))), 0
    version = u32(page, 8)
    if version != KERNEL_TABLE_VERSION:
        return False, "kernel tables version %d" % version, 0
    page_count = u32(page, 12)
    process_count = u32(page, 16)
    if first + page_count - 1 > last:
        return False, ("kernel tables of %d pages run past the entry"
                       % page_count), 0

    location = first + 1
    for i in range(process_count):
        record = log.page(location)
        if location > first + page_count - 1 \
           or u64(record, 0) != PROCESS_RECORD_MAGIC:
            return False, ("location %d: expected process record %d of %d"
                           % (location, i + 1, process_count)), 0
        oid = u64(record, 8)
        name = record[16:32].decode("latin-1").rstrip(" \0")
        state = u32(record, 32)
        last_cap, cap_count, cap_pages = struct.unpack_from("<III",
                                                            record, 56)
        if cap_pages != (cap_count + CAPS_PER_PAGE - 1) // CAPS_PER_PAGE \
           or cap_count > last_cap:
            return False, ("process %d (%s): %d caps up to %d do not fit "
                           "%d cap pages"
                           % (oid, name, cap_count, last_cap, cap_pages)), 0
        if verbose:
            print("    process %d %-16s %-11s %3d caps"
                  % (oid, name or "(no name)",
                     PROCESS_STATES[state] if state < len(PROCESS_STATES)
                     else "state %d" % state, cap_count))
        location += 1 + cap_pages

    if location != first + page_count:
        return False, ("kernel tables hold %d pages of records, header "
                       "says %d" % (location - first, page_count)), 0

    return True, ("kernel: %d process%s in %d pages"
                  % (process_count, "" if process_count == 1 else "es",
                     page_count)), page_count


def check_entry(log, first, last, verbose):
    """Check the checkpoint stored in pages first .. last.

    Returns (ok, description)."""
    ok, kernel, kernel_pages = check_kernel_tables(log, first, last,
                                                   verbose)
    if not ok:
        return False, kernel
    ok, memory = check_memory(log, first + kernel_pages, last, verbose)
    return ok, kernel + "; " + memory


def check_memory(log, first, last, verbose):
    """Check a memory checkpoint stored in pages first .. last.

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
        listed += u32(page, 12)
        if verbose:
            print("    header page at %d lists %d pages"
                  % (location, u32(page, 12)))
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
