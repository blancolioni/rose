#!/usr/bin/env python3
"""Make the checkpoint log partition of a disk image smaller.

    set-log-size.py IMAGE PAGES

Moves the end of the log partition in both copies of the image's GPT,
so that the log has PAGES pages, its header included, and updates both
copies' CRCs, which Rose.Devices.GPT checks.  The space after the new
end is left unused.

It is for the test of the memory server's cleaning of the log (#952),
scripts/test-log-clean.  A test disk's log is half of the disk, which an
idle system would take days to fill, and cleaning starts only once the
log is half full (Rose.Limits.Clean_Start_Percent).  Shrinking the log
after an install makes it start at once.

Every entry of the log has to be within the new size: it refuses to cut
off one, or a log that has wrapped around.  The log is found as
check-log.py finds it.
"""

import argparse
import importlib.util
import os
import struct
import sys
import zlib

# Loading check-log.py as a module would leave its bytecode in
# scripts/__pycache__.
sys.dont_write_bytecode = True
HERE = os.path.dirname(os.path.abspath(__file__))
spec = importlib.util.spec_from_file_location(
    "check_log", os.path.join(HERE, "check-log.py"))
check_log = importlib.util.module_from_spec(spec)
spec.loader.exec_module(check_log)

BLOCK_SIZE = check_log.BLOCK_SIZE
BLOCKS_PER_PAGE = check_log.PAGE_SIZE // BLOCK_SIZE


def update_copy(image, header_lba, first_block, last_block):
    """Set the log partition's last block in the GPT copy whose header is
    at header_lba, and its CRCs.  False if there is no header there, or
    no log partition in its entries."""
    image.seek(header_lba * BLOCK_SIZE)
    header = bytearray(image.read(BLOCK_SIZE))
    if check_log.u64(header, 0) != check_log.GPT_MAGIC:
        return False
    header_size = check_log.u32(header, 12)
    entries_lba = check_log.u64(header, 72)
    count = check_log.u32(header, 80)
    entry_size = check_log.u32(header, 84)

    image.seek(entries_lba * BLOCK_SIZE)
    table = bytearray(image.read(count * entry_size))
    found = False
    for i in range(count):
        offset = i * entry_size
        if (check_log.u64(table, offset), check_log.u64(table, offset + 8)) \
           == check_log.LOG_TYPE \
           and check_log.u64(table, offset + 32) == first_block:
            struct.pack_into("<Q", table, offset + 40, last_block)
            found = True
    if not found:
        return False

    struct.pack_into("<I", header, 88, zlib.crc32(table) & 0xFFFFFFFF)
    struct.pack_into("<I", header, 16, 0)
    struct.pack_into("<I", header, 16,
                     zlib.crc32(bytes(header[:header_size])) & 0xFFFFFFFF)

    image.seek(entries_lba * BLOCK_SIZE)
    image.write(table)
    image.seek(header_lba * BLOCK_SIZE)
    image.write(header)
    return True


def main():
    parser = argparse.ArgumentParser(
        description="Make the log partition of a disk image smaller.")
    parser.add_argument("image")
    parser.add_argument("pages", type=int,
                        help="the log's new size in pages, its header "
                        "included")
    args = parser.parse_args()

    with open(args.image, "r+b") as image:
        partition = check_log.find_log_partition(image)
        if partition is None:
            print("%s: no GPT log partition" % args.image)
            return 1
        first_block, last_block = partition
        log = check_log.Log(image, first_block, last_block)
        header = log.page(0)
        if check_log.u64(header, 0) != check_log.LOG_MAGIC:
            print("the log partition has no log header")
            return 1

        if not 2 <= args.pages <= log.page_count:
            print("the log has %d pages; it can't have %d"
                  % (log.page_count, args.pages))
            return 1

        end = 0
        for location, first, last in check_log.walk_entries(log, header):
            if last < location:
                print("the log has wrapped around; it can't be cut")
                return 1
            end = max(end, last)
        if end >= args.pages:
            print("the log's entries reach location %d; it can't have %d "
                  "pages" % (end, args.pages))
            return 1

        new_last = first_block + args.pages * BLOCKS_PER_PAGE - 1
        image.seek(0, os.SEEK_END)
        disk_last = image.tell() // BLOCK_SIZE - 1
        if not update_copy(image, 1, first_block, new_last) \
           or not update_copy(image, disk_last, first_block, new_last):
            print("%s: a GPT copy has no log partition" % args.image)
            return 1

        print("log: blocks %d..%d, %d pages; its entries end at location %d"
              % (first_block, new_last, args.pages, end))
    return 0


if __name__ == "__main__":
    sys.exit(main())
