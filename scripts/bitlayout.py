#!/usr/bin/env python3
"""Draw bit-layout diagrams for the Rose book as SVG.

Each input YAML file holds a set of diagrams under 'diagrams'.  A
diagram is a list of rows, and each row is one word, given either as
a list of fields from the lowest bit up, or as {words: n, name: ...}
for a field that spans whole words.  A field is [bits, name] or
[bits, name, const]; const fields hold a fixed value and are shaded.

Bit numbers are drawn above the first row, at each field boundary,
with the highest bit on the left.  One SVG is written per diagram,
named after its key.

Usage: bitlayout.py -o OUTDIR FILE.yaml...
"""

import argparse
import os
import re
import sys
from xml.sax.saxutils import escape

import yaml

BIT_WIDTH = 18
ROW_HEIGHT = 30
HEADER_HEIGHT = 16
MARGIN = 2
FONT_SIZE = 13
HEADER_FONT_SIZE = 10
CHAR_WIDTH = 0.6 * FONT_SIZE    # rough average, for overflow warnings
CONST_FILL = "#e4e4e4"


class LayoutError(Exception):
    pass


def parse_field(where, value):
    if not isinstance(value, list) or len(value) not in (2, 3):
        raise LayoutError(f"{where}: field must be [bits, name] "
                          f"or [bits, name, const], not {value!r}")
    bits, name = value[0], value[1]
    if len(value) == 3 and value[2] != "const":
        raise LayoutError(f"{where}: unknown field option {value[2]!r}")
    if not isinstance(bits, int) or bits < 1:
        raise LayoutError(f"{where}: bad bit count {bits!r}")
    if not isinstance(name, str):
        raise LayoutError(f"{where}: field name {name!r} must be a string "
                          f"(quote constants such as \"0100\")")
    return bits, name, len(value) == 3


def parse_row(where, word, value):
    """Return ('fields', [(lo, hi, name, const)]) or ('words', n, name)."""
    if isinstance(value, dict):
        words, name = value.get("words"), value.get("name")
        if set(value) != {"words", "name"} \
                or not isinstance(words, int) or words < 1 \
                or not isinstance(name, str):
            raise LayoutError(f"{where}: expected {{words: n, name: ...}}, "
                              f"not {value!r}")
        return ("words", words, name)
    if not isinstance(value, list) or not value:
        raise LayoutError(f"{where}: row must be a list of fields")
    fields = []
    lo = 0
    for index, item in enumerate(value):
        bits, name, const = parse_field(f"{where}, field {index + 1}", item)
        fields.append((lo, lo + bits - 1, name, const))
        lo += bits
    if lo != word:
        raise LayoutError(f"{where}: fields add up to {lo} bits, "
                          f"not {word}")
    return ("fields", fields)


def parse_diagram(name, word, value):
    if not isinstance(value, dict) or not isinstance(value.get("rows"), list):
        raise LayoutError(f"{name}: diagram must have a list of rows")
    return [parse_row(f"{name}, row {index + 1}", word, row)
            for index, row in enumerate(value["rows"])]


def bit_left(word, bit):
    return MARGIN + (word - 1 - bit) * BIT_WIDTH


def text(x, y, size, label, extra=""):
    # Baseline offset rather than dominant-baseline, which not every
    # SVG renderer supports.
    return (f'<text x="{x:g}" y="{y + 0.35 * size:g}" font-size="{size}" '
            f'text-anchor="middle"{extra}>{escape(label)}</text>')


def box(name, x, y, width, height, label, const):
    if len(label) * CHAR_WIDTH > width - 4:
        print(f"warning: {name}: '{label}' may not fit its box",
              file=sys.stderr)
    fill = CONST_FILL if const else "#ffffff"
    return [f'<rect x="{x:g}" y="{y:g}" width="{width:g}" '
            f'height="{height:g}" fill="{fill}" stroke="#000000"/>',
            text(x + width / 2, y + height / 2, FONT_SIZE, label)]


def render(name, title, word, rows):
    width = 2 * MARGIN + word * BIT_WIDTH
    height = 2 * MARGIN + HEADER_HEIGHT \
        + ROW_HEIGHT * sum(row[1] if row[0] == "words" else 1
                           for row in rows)
    parts = [f'<svg xmlns="http://www.w3.org/2000/svg" '
             f'width="{width}" height="{height}" '
             f'viewBox="0 0 {width} {height}" '
             f'font-family="sans-serif" fill="#000000">',
             f'<title>{escape(title)}</title>',
             f'<rect width="{width}" height="{height}" fill="#ffffff"/>']

    first = next((row[1] for row in rows if row[0] == "fields"), None)
    ticks = {0, word - 1} if first is None \
        else {bit for lo, hi, _, _ in first for bit in (lo, hi)}
    for bit in sorted(ticks):
        parts.append(text(bit_left(word, bit) + BIT_WIDTH / 2,
                          MARGIN + HEADER_HEIGHT / 2,
                          HEADER_FONT_SIZE, str(bit)))

    y = MARGIN + HEADER_HEIGHT
    for row in rows:
        if row[0] == "fields":
            for lo, hi, label, const in row[1]:
                parts += box(name, bit_left(word, hi), y,
                             (hi - lo + 1) * BIT_WIDTH, ROW_HEIGHT,
                             label, const)
            y += ROW_HEIGHT
        else:
            _, words, label = row
            parts += box(name, MARGIN, y, word * BIT_WIDTH,
                         words * ROW_HEIGHT, label, False)
            y += words * ROW_HEIGHT

    parts.append("</svg>")
    return "\n".join(parts) + "\n"


def process(path, outdir):
    with open(path, encoding="utf-8") as f:
        data = yaml.safe_load(f)
    if not isinstance(data, dict) or not isinstance(data.get("diagrams"),
                                                     dict):
        raise LayoutError(f"{path}: expected a 'diagrams' mapping")
    word = data.get("word", 32)
    for name, value in data["diagrams"].items():
        if not re.fullmatch(r"[a-z0-9][a-z0-9-]*", str(name)):
            raise LayoutError(f"{path}: bad diagram name {name!r}")
        rows = parse_diagram(name, word, value)
        svg = render(name, value.get("title", name), word, rows)
        with open(os.path.join(outdir, f"{name}.svg"), "w",
                  encoding="utf-8", newline="\n") as f:
            f.write(svg)


def main():
    parser = argparse.ArgumentParser(description=__doc__.splitlines()[0])
    parser.add_argument("-o", "--output", required=True,
                        help="directory for the SVG files")
    parser.add_argument("files", nargs="+", help="layout YAML files")
    args = parser.parse_args()
    os.makedirs(args.output, exist_ok=True)
    try:
        for path in args.files:
            process(path, args.output)
    except LayoutError as e:
        print(f"bitlayout: {e}", file=sys.stderr)
        sys.exit(1)


if __name__ == "__main__":
    main()
