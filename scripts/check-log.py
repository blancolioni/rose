#!/usr/bin/env python3
"""Check the checkpoint log on a Rose disk image.

Finds the log partition in the image's GPT, reads the log header, and
walks the entries the way the log server's startup scan does.  Each
entry should be a memory checkpoint: the check is that it starts with
the memory server's header pages, and holds exactly those pages plus
the pages they list.

After walking the entries it also looks at the location after the last
one, and reports anything there that looks like an entry header, such
as an entry left over from before a reset.

The layouts are those of src/servers/log/src/log-header.ads,
src/servers/mem/src/mem-checkpoints.adb and
src/library/kernelapi/devices/rose-devices-gpt.adb.

Usage: check-log.py [-v] IMAGE

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


def check_checkpoint(log, first, last, verbose):
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
            # Every entry is a memory checkpoint for now; the kernel's
            # tables (#117) will need their own check here.
            return False, ("location %d: expected a checkpoint header "
                           "page, found %s"
                           % (location,
                              "an empty page" if not any(page)
                              else "something else"))
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
    description = ("%d header page%s, %d data pages"
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
                        help="describe each checkpoint header page")
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
            ok, description = check_checkpoint(log, first, last,
                                               args.verbose)
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
