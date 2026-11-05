#!/usr/bin/env python3
"""Read the text on a framebuffer screen, from QEMU.

The console on the screen's surface (surface-console, #653) draws text in
8 by 16 cells of the Spleen font.  This asks QEMU's monitor for a screen
dump, and reads each cell by matching its pixels with the font's glyphs,
which it takes from src/library/drawing/rose-drawing-text-font.ads.  A
pixel is part of a glyph if it isn't the background, black.  A cell that
matches no glyph exactly is matched without its bottom two rows of pixels,
where the cursor is drawn.  A cell in reverse video (#734) is matched
with its pixels inverted, and a cell that matches nothing is "?".

Usage: fb-text.py MONITOR

MONITOR is QEMU's monitor socket.  It writes the screen's text, one row of
cells to a line, with the spaces at the end of each line removed, as
qemu-screen.py does for the VGA text page.
"""

import os
import re
import socket
import sys
import tempfile
import time

HERE = os.path.dirname(os.path.abspath(__file__))
FONT = os.path.join(HERE, "..", "src", "library", "drawing",
                    "rose-drawing-text-font.ads")


def load_font():
    """Map each glyph's 16 rows to its character, and its first 14 rows
    to its character where that is unambiguous."""
    text = open(FONT).read()
    full = {}
    partial = {}
    for code, rows in re.findall(r"Character'Val \((\d+)\) =>\s*\(([^)]*)\)",
                                 text):
        values = tuple(int(v, 16) for v in re.findall(r"16#([0-9A-F]+)#",
                                                     rows))
        if len(values) != 16:
            continue
        ch = chr(int(code))
        full.setdefault(values, ch)
        partial.setdefault(values[:14], set()).add(ch)
    full[(0,) * 16] = " "
    partial.setdefault((0,) * 14, set()).add(" ")
    return full, {k: v.pop() for k, v in partial.items() if len(v) == 1}


def screen_dump(monitor):
    """The screen, from QEMU, as (width, height, pixels)."""
    handle, path = tempfile.mkstemp(suffix=".ppm")
    os.close(handle)
    try:
        c = socket.socket(socket.AF_UNIX)
        c.connect(monitor)
        c.sendall(("screendump " + path + "\n").encode())
        for _ in range(50):
            time.sleep(0.1)
            if os.path.getsize(path) > 0:
                time.sleep(0.2)
                break
        c.close()
        data = open(path, "rb").read()
    finally:
        os.unlink(path)
    magic, size, _depth, pixels = data.split(b"\n", 3)
    width, height = map(int, size.split())
    return width, height, pixels


def main():
    if len(sys.argv) != 2:
        print("usage: fb-text.py MONITOR", file=sys.stderr)
        return 2

    full, partial = load_font()
    try:
        width, height, pixels = screen_dump(sys.argv[1])
    except (OSError, ValueError):
        return 1

    for row in range(height // 16):
        line = []
        for column in range(width // 8):
            rows = []
            for y in range(16):
                offset = ((row * 16 + y) * width + column * 8) * 3
                bits = 0
                for x in range(8):
                    if any(pixels[offset + x * 3:offset + x * 3 + 3]):
                        bits |= 0x80 >> x
                rows.append(bits)
            rows = tuple(rows)
            inverse = tuple(0xFF ^ bits for bits in rows)
            line.append(full.get(rows) or partial.get(rows[:14])
                        or full.get(inverse) or partial.get(inverse[:14], "?"))
        print("".join(line).rstrip())
    return 0


if __name__ == "__main__":
    sys.exit(main())
