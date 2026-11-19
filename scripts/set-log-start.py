#!/usr/bin/env python3
"""Start a new, empty checkpoint log near the end of the log partition.

    set-log-start.py IMAGE [--from-end N]

Writes a new log header to the log partition of the disk image IMAGE:
the next generation, so that none of the old entries belongs to the
log, with its first entry N locations before the end of the partition
(50 by default) and sequence number 1.  The log server then writes the
next entry there, and the entries after it wrap around to location 1.

It is for the test of a log that wraps around (#951), scripts/test-log-wrap:
nothing frees space in the log until the memory server cleans it (#952),
so a log that starts at 1 never reaches the end and wraps.

The log is found as check-log.py finds it, and the layout of the header
is Log.Header.Log_Header_Type, version 3.
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


def main():
    parser = argparse.ArgumentParser(
        description="Start a new, empty log near the end of the partition.")
    parser.add_argument("image")
    parser.add_argument("--from-end", type=int, default=50,
                        help="how many locations before the end of the "
                        "partition the log starts (default 50)")
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

        last_location = log.page_count - 1
        if not 0 <= args.from_end < last_location:
            print("--from-end %d: the log's last location is %d"
                  % (args.from_end, last_location))
            return 1

        generation = check_log.log_generation(header) + 1
        first_entry = last_location - args.from_end
        new_header = bytearray(check_log.PAGE_SIZE)
        struct.pack_into("<QQ", new_header, 0, check_log.LOG_MAGIC,
                         generation)
        struct.pack_into("<QQ", new_header, 4080, 1, first_entry)
        image.seek(log.base)
        image.write(new_header)
        print("log: generation %d, starting at location %d of 1..%d"
              % (generation, first_entry, last_location))
    return 0


if __name__ == "__main__":
    sys.exit(main())
