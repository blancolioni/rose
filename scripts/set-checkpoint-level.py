#!/usr/bin/env python3
"""Change the interface level that a checkpoint records, on a disk image.

    set-checkpoint-level.py IMAGE [--add N]

Adds N (1 by default, and it may be negative) to the interface level in
the kernel tables of the last entry of the checkpoint log, and prints the
level before and after.  It is for testing that the restore server
refuses a checkpoint written with interfaces that its boot set doesn't
have (#375): scripts/test-refuse raises the level, and then lowers it
again.

The log is found and walked as check-log.py does, and the layout of the
kernel tables' header is Rose.Checkpoints.Kernel_Table_Header, version 9
or later: the interfaces follow the 32 saved interrupts.
"""

import argparse
import importlib.util
import os
import struct
import sys

# Loading check-log.py as a module would leave its bytecode in
# scripts/__pycache__.
sys.dont_write_bytecode = True

HERE = os.path.dirname(os.path.abspath(__file__))
spec = importlib.util.spec_from_file_location(
    "check_log", os.path.join(HERE, "check-log.py"))
check_log = importlib.util.module_from_spec(spec)
spec.loader.exec_module(check_log)

INTERFACES_OFFSET = 40 + check_log.MAX_SAVED_INTERRUPTS * 16
FIRST_VERSION = 9


def main():
    parser = argparse.ArgumentParser(
        description="Change the interface level of the last checkpoint.")
    parser.add_argument("image")
    parser.add_argument("--add", type=int, default=1,
                        help="what to add to the level (default 1)")
    args = parser.parse_args()

    with open(args.image, "r+b") as image:
        partition = check_log.find_log_partition(image)
        if partition is None:
            print("%s: no GPT log partition" % args.image)
            return 1
        log = check_log.Log(image, *partition)
        header = log.page(0)
        if check_log.u64(header, 0) != check_log.LOG_MAGIC:
            print("%s: the log partition has no log header" % args.image)
            return 1
        generation = check_log.log_generation(header)

        last_first = None
        last_location = None
        location = 1
        while True:
            fields = check_log.entry_fields(log.page(location))
            if fields is None:
                break
            version, flags, entry_generation, first, last = fields
            if version == 2 and entry_generation != generation:
                break
            last_first = first
            last_location = location
            location = last + 1

        if last_first is None:
            print("%s: the log has no entries" % args.image)
            return 1

        page = log.page(last_first)
        if check_log.u64(page, 0) != check_log.KERNEL_TABLE_MAGIC:
            print("location %d: expected the kernel's tables" % last_first)
            return 1
        version = check_log.u32(page, 8)
        if version < FIRST_VERSION:
            print("location %d: kernel tables version %d have no interface "
                  "level" % (last_first, version))
            return 1

        major, minor, level = struct.unpack_from("<HHI", page,
                                                 INTERFACES_OFFSET)
        new_level = level + args.add
        if not 0 <= new_level <= 0xFFFF_FFFF:
            print("level %d + %d is out of range" % (level, args.add))
            return 1

        image.seek(log.base + last_first * check_log.PAGE_SIZE
                   + INTERFACES_OFFSET + 4)
        image.write(struct.pack("<I", new_level))
        # An entry is named by the location of its header, as the
        # restore server and check-log.py name it.
        print("entry at %d: interfaces %d.%d level %d, now %d"
              % (last_location, major, minor, level, new_level))
    return 0


if __name__ == "__main__":
    sys.exit(main())
