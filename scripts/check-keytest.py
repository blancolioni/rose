#!/usr/bin/env python3
"""Check the screen that keytest drew (#734).

usage: check-keytest.py SCREEN KEY DX DY

SCREEN is a file with the screen's text, one row to a line, as
qemu-screen.py and fb-text.py write it.  Keytest has drawn a + in each
corner of a screen of any size, and an @ at its centre, and the arrow
keys have moved the @ by DX columns and DY rows.  The last key that was
typed is KEY, as keytest names it, such as Left.

This checks that the title gives the screen's size, that a + is in each
corner of a screen of that size, that the bottom line names KEY and gives
the @'s new position, and that the @ is there.  The screen may have lines
above keytest's, such as the kernel's status line on the VGA text page.
It exits with 0 if everything is as it should be.
"""

import re
import sys


def main():
    if len(sys.argv) != 5:
        print(__doc__, file=sys.stderr)
        return 2

    lines = open(sys.argv[1]).read().split("\n")
    key, dx, dy = sys.argv[2], int(sys.argv[3]), int(sys.argv[4])

    title = [m for m in (re.search(r"keytest (\d+)x(\d+)", line)
                         for line in lines) if m]
    if not title:
        print("keytest: no title with the screen's size")
        return 1
    columns, rows = int(title[0].group(1)), int(title[0].group(2))

    def is_edge(line):
        return (len(line) == columns and line.startswith("+")
                and line.endswith("+"))

    tops = [i for i, line in enumerate(lines) if is_edge(line)]
    if not tops:
        print("keytest: no top row with a + in each corner")
        return 1
    top = tops[0]
    if top + rows - 1 >= len(lines) or not is_edge(lines[top + rows - 1]):
        print("keytest: no bottom row with a + in each corner, %d rows "
              "below the top" % (rows - 1))
        return 1

    status = [m for m in (re.search(r"key (\S+) at (\d+),(\d+)", line)
                          for line in lines) if m]
    if not status:
        print("keytest: no status line")
        return 1
    name, x, y = status[-1].group(1), int(status[-1].group(2)), \
        int(status[-1].group(3))

    expected = (columns // 2 + dx, rows // 2 + dy)
    print("keytest: %dx%d, last key %s, @ at %d,%d"
          % (columns, rows, name, x, y))
    if name != key:
        print("keytest: the last key is %s, not %s" % (name, key))
        return 1
    if (x, y) != expected:
        print("keytest: the @ should be at %d,%d" % expected)
        return 1
    row = lines[top + y]
    if len(row) <= x or row[x] != "@":
        print("keytest: no @ at %d,%d: %r" % (x, y, row))
        return 1
    return 0


if __name__ == "__main__":
    sys.exit(main())
