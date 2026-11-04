#!/usr/bin/env python3
"""Check a QEMU screen dump for the screen test's pattern.

Tests.Screen in run_self_tests draws a pattern over the whole screen
(#582): each pixel's red is its column mod 256, its green its row mod 256,
and its blue 0x80.  This samples a grid of pixels in a screen dump, a PPM
file from QEMU's screendump command, and counts those that match.

Usage: check-screen.py DUMP

Exits with status 0 if at least 95% of the samples match, and 1 if fewer
do or the file isn't a screen dump.
"""

import sys


def main():
    if len(sys.argv) != 2:
        print("usage: check-screen.py DUMP")
        return 2

    try:
        data = open(sys.argv[1], "rb").read()
        magic, size, depth, pixels = data.split(b"\n", 3)
        width, height = map(int, size.split())
    except (OSError, ValueError):
        print("%s: not a screen dump" % sys.argv[1])
        return 1

    if magic != b"P6" or depth != b"255" or len(pixels) < width * height * 3:
        print("%s: not a screen dump" % sys.argv[1])
        return 1

    total = matched = 0
    for y in range(5, height, 37):
        for x in range(3, width, 41):
            offset = (y * width + x) * 3
            total += 1
            if tuple(pixels[offset:offset + 3]) == (x % 256, y % 256, 0x80):
                matched += 1

    print("%dx%d: %d of %d samples show the pattern"
          % (width, height, matched, total))
    return 0 if total > 0 and matched * 100 >= total * 95 else 1


if __name__ == "__main__":
    sys.exit(main())
