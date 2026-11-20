#!/usr/bin/env python3
"""Make a disk image's checkpoint log look as a crash would have left it.

    make-log-crash.py IMAGE before|between|after

The commit that moves the log's start writes two things: the new entry's
header, which records the new start (#953), and then the log's header,
which moves its first entry there and frees the space before it (#951,
#952).  The system can stop before the first write, between the two, or
after both.  Given an image whose log start has been moved, this finds
the last entry E that moved it, and makes the image as it would be had
the system stopped:

  before   before E's header was written: the log's header starts the log
           where it did before E, and E's header is erased, so the log
           ends with the entry before E, which restore has to choose;
  between  after E's header and before the log's: the log's header starts
           the log where it did before E, the entry after E is erased, and
           the log server has to finish the move when it starts;
  after    after both: the entry after E is erased.

The entries before E's start were freed, so they are only still there
if nothing has written over them since; it refuses if something has.
It prints the location of the entry that restore has to choose:

    expect checkpoint at LOCATION

It is for scripts/test-log-crash.  The layouts are those that
check-log.py reads.
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

FIRST_SEQUENCE_OFFSET = 4080
FIRST_ENTRY_OFFSET = 4088


class Image:
    """The log of a disk image, with writes."""

    def __init__(self, image, first_block, last_block):
        self.image = image
        self.log = check_log.Log(image, first_block, last_block)

    def write_page(self, location, data):
        self.image.seek(self.log.base
                        + self.log.physical(location) * check_log.PAGE_SIZE)
        self.image.write(data)

    def erase_entry(self, location):
        """Erase the entry header at location, so that a scan stops
        there."""
        page = bytearray(self.log.page(location))
        page[0:8] = bytes(8)
        self.write_page(location, bytes(page))


def entries(log, start, sequence, generation):
    """The entries of the log from location start, whose entry has
    sequence number sequence: a list of (location, last, sequence, start
    sequence), in order."""
    result = []
    location = start
    while True:
        page = log.page(location)
        fields = check_log.entry_fields(page)
        if fields is None:
            break
        version, _, entry_generation, first, last, entry_sequence = fields
        if version != 3 or entry_generation != generation \
           or entry_sequence != sequence or first != log.next(location):
            break
        result.append((location, last, sequence, check_log.entry_start(page)))
        location = log.next(last)
        sequence += 1
        if location == start:
            break
    return result


def find_entry_before(log, location, sequence, generation):
    """Find, going back from location, the header of the entry with
    sequence number sequence whose entries lead on to location.  Return
    its location, or None."""
    at = location
    for _ in range(log.page_count - 1):
        at = at - 1 if at > 1 else log.page_count - 1
        fields = check_log.entry_fields(log.page(at))
        if fields is None:
            continue
        version, _, entry_generation, _, _, entry_sequence = fields
        if version == 3 and entry_generation == generation \
           and entry_sequence == sequence:
            chain = entries(log, at, sequence, generation)
            if any(where == location for where, _, _, _ in chain):
                return at
            return None
    return None


def main():
    parser = argparse.ArgumentParser(
        description="Make a log look as a crash would have left it.")
    parser.add_argument("image")
    parser.add_argument("case", choices=("before", "between", "after"))
    args = parser.parse_args()

    with open(args.image, "r+b") as image:
        partition = check_log.find_log_partition(image)
        if partition is None:
            print("%s: no GPT log partition" % args.image)
            return 1
        disk = Image(image, *partition)
        log = disk.log
        header = bytearray(log.page(0))
        if check_log.u64(header, 0) != check_log.LOG_MAGIC:
            print("the log partition has no log header")
            return 1
        generation = check_log.log_generation(header)
        start, start_sequence = check_log.log_start(header)

        walked = entries(log, start, start_sequence, generation)
        moved = [i for i in range(1, len(walked))
                 if walked[i][3] != walked[i - 1][3]]
        if not moved:
            print("no entry in the log moved its start")
            return 1
        e = moved[-1]
        location, last, sequence, new_start = walked[e]
        old_start = walked[e - 1][3]
        if new_start != start_sequence:
            print("the entry at %d moved the start to sequence %d, but the "
                  "log starts at %d" % (location, new_start, start_sequence))
            return 1
        print("the entry at %d, sequence %d, moved the log's start from "
              "sequence %d to %d" % (location, sequence, old_start,
                                     new_start))

        if args.case in ("before", "between"):
            old_location = find_entry_before(log, start, old_start,
                                             generation)
            if old_location is None:
                print("the entries from sequence %d have been written over"
                      % old_start)
                return 1
            struct.pack_into("<Q", header, FIRST_SEQUENCE_OFFSET, old_start)
            struct.pack_into("<Q", header, FIRST_ENTRY_OFFSET, old_location)
            disk.write_page(0, bytes(header))
            print("the log starts at %d again" % old_location)

        if args.case == "before":
            disk.erase_entry(location)
            print("erased the entry at %d" % location)
            expect = walked[e - 1][0]
        else:
            if e + 1 < len(walked):
                disk.erase_entry(walked[e + 1][0])
                print("erased the entry at %d" % walked[e + 1][0])
            expect = location

        print("expect checkpoint at %d" % expect)
    return 0


if __name__ == "__main__":
    sys.exit(main())
