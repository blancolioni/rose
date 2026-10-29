#!/usr/bin/env python3
"""Draw the Rose book's process diagrams as SVG.

The input YAML file describes the processes running after each boot,
and which of them call which (see doc/rose-book/diagrams/processes.yaml
for the format).  One SVG is written for each variant, called
processes-VARIANT.svg.

The SVGs use only plain attributes, with no CSS or markers, so that
asciidoctor-pdf can draw them as well as browsers.

Usage: process-diagram.py -o OUTDIR FILE.yaml
"""

import argparse
import os
from xml.sax.saxutils import escape

import yaml

BOX_WIDTH = 120
BOX_HEIGHT = 40
FONT_SIZE = 13
NOTE_SIZE = 11
LABEL_SIZE = 11
CHAR_WIDTH = 0.58 * LABEL_SIZE  # rough average, for label backgrounds
ARROW_LENGTH = 9
ARROW_WIDTH = 7

INK = "#1a1e1b"
MUTED = "#59625b"
ROSE = "#a3121f"
BOOT_BAND = "#eef0ec"
PERSISTENT_BAND = "#fbeff0"
BOX = "#ffffff"
FADED = "#bcc2bb"


class Box:

    def __init__(self, spec):
        self.x, self.y = spec["at"]
        self.width = spec.get("width", BOX_WIDTH)
        self.kind = spec["kind"]
        self.label = spec["label"]
        self.note = str(spec.get("note", ""))
        self.notes = spec.get("notes", {})
        self.when = spec.get("when")

    def left(self):
        return self.x - self.width / 2

    def right(self):
        return self.x + self.width / 2

    def top(self):
        return self.y - BOX_HEIGHT / 2

    def bottom(self):
        return self.y + BOX_HEIGHT / 2

    def edge_toward(self, x, y):
        """Where the line from the centre toward (x, y) leaves the box."""
        dx, dy = x - self.x, y - self.y
        scales = []
        if dx:
            scales.append(self.width / 2 / abs(dx))
        if dy:
            scales.append(BOX_HEIGHT / 2 / abs(dy))
        t = min(scales)
        return self.x + dx * t, self.y + dy * t


def route(source, target, via):
    """The points of the line from source to target."""
    if via:
        first, last = via[0], via[-1]
        if source.left() < first[0] < source.right() \
           and first[1] > source.bottom():
            start = (first[0], source.bottom())
        else:
            start = source.edge_toward(*first)
        if target.left() < last[0] < target.right() \
           and last[1] < target.top():
            end = (last[0], target.top())
        else:
            end = target.edge_toward(*last)
        return [start] + [tuple(p) for p in via] + [end]
    if source.y == target.y:
        if target.x < source.x:
            return [(source.left(), source.y), (target.right(), target.y)]
        return [(source.right(), source.y), (target.left(), target.y)]
    down = target.y > source.y
    for column, other in ((target.x, source), (source.x, target)):
        if other.left() < column < other.right():
            if down:
                return [(column, source.bottom()), (column, target.top())]
            return [(column, source.top()), (column, target.bottom())]
    return [source.edge_toward(target.x, target.y),
            target.edge_toward(source.x, source.y)]


def arrow_head(points, colour):
    (x0, y0), (x1, y1) = points[-2], points[-1]
    length = ((x1 - x0) ** 2 + (y1 - y0) ** 2) ** 0.5
    ux, uy = (x1 - x0) / length, (y1 - y0) / length
    bx, by = x1 - ux * ARROW_LENGTH, y1 - uy * ARROW_LENGTH
    px, py = -uy * ARROW_WIDTH / 2, ux * ARROW_WIDTH / 2
    corners = [(x1, y1), (bx + px, by + py), (bx - px, by - py)]
    text = " ".join(f"{x:.1f},{y:.1f}" for x, y in corners)
    return f'<polygon points="{text}" fill="{colour}"/>'


def shorten(points):
    """Stop the line where the arrow head starts."""
    (x0, y0), (x1, y1) = points[-2], points[-1]
    length = ((x1 - x0) ** 2 + (y1 - y0) ** 2) ** 0.5
    t = (length - ARROW_LENGTH + 1) / length
    return points[:-1] + [(x0 + (x1 - x0) * t, y0 + (y1 - y0) * t)]


def label_at(points):
    """The middle of the longest segment."""
    segments = list(zip(points, points[1:]))
    (x0, y0), (x1, y1) = max(
        segments,
        key=lambda s: abs(s[1][0] - s[0][0]) + abs(s[1][1] - s[0][1]))
    return (x0 + x1) / 2, (y0 + y1) / 2


def draw(spec, variant):
    width, height = spec["size"]
    boxes = {name: Box(p) for name, p in spec["processes"].items()}
    bands = spec["bands"]
    persistent, boot = bands["persistent"], bands["boot"]
    parts = [f'<svg xmlns="http://www.w3.org/2000/svg" '
             f'width="{width}" height="{height}" '
             f'viewBox="0 0 {width} {height}" '
             f'font-family="sans-serif">',
             f'<rect width="{width}" height="{height}" fill="#ffffff"/>']

    def band(top, bottom, fill, title, colour):
        parts.append(f'<rect x="8" y="{top}" width="{width - 16}" '
                     f'height="{bottom - top}" rx="10" fill="{fill}"/>')
        parts.append(f'<text x="24" y="{top + 24}" font-size="{FONT_SIZE}" '
                     f'font-weight="bold" fill="{colour}">'
                     f'{escape(title)}</text>')

    band(persistent["top"], persistent["bottom"], PERSISTENT_BAND,
         spec["variants"][variant], ROSE)
    band(boot["top"], boot["bottom"], BOOT_BAND, boot["title"], MUTED)

    labels = []
    for call in spec["calls"]:
        if isinstance(call, list):
            call = {"from": call[0], "to": call[1]}
        when = call.get("when")
        if when and when != variant:
            continue
        points = route(boxes[call["from"]], boxes[call["to"]],
                       call.get("via"))
        colour = ROSE if when else INK
        dash = ' stroke-dasharray="5 3"' if when else ""
        line = " ".join(f"{x:.1f},{y:.1f}" for x, y in shorten(points))
        parts.append(f'<polyline points="{line}" fill="none" '
                     f'stroke="{colour}" stroke-width="1.2"{dash}/>')
        parts.append(arrow_head(points, colour))
        if "label" in call:
            x, y = label_at(points)
            fill = BOOT_BAND if y > boot["top"] else PERSISTENT_BAND
            labels.append((x, y, call["label"], fill))

    for x, y, text, fill in labels:
        w = len(text) * CHAR_WIDTH + 8
        parts.append(f'<rect x="{x - w / 2:.1f}" y="{y - 8:.1f}" '
                     f'width="{w:.1f}" height="16" fill="{fill}"/>')
        parts.append(f'<text x="{x:.1f}" y="{y + 4:.1f}" '
                     f'font-size="{LABEL_SIZE}" text-anchor="middle" '
                     f'fill="{MUTED}">{escape(text)}</text>')

    for box in boxes.values():
        faded = box.when and box.when != variant
        stroke = FADED if faded else ROSE if box.kind == "persistent" else INK
        stroke_width = 1.6 if box.kind == "persistent" else 1.2
        dash = ' stroke-dasharray="5 3"' if box.when else ""
        name_colour = FADED if faded else INK
        note_colour = FADED if faded else MUTED
        parts.append(f'<rect x="{box.left()}" y="{box.top()}" '
                     f'width="{box.width}" height="{BOX_HEIGHT}" rx="6" '
                     f'fill="{BOX}" stroke="{stroke}" '
                     f'stroke-width="{stroke_width}"{dash}/>')
        note = box.notes.get(variant, box.note)
        name_y = box.y - 2 if note else box.y + 5
        parts.append(f'<text x="{box.x}" y="{name_y}" '
                     f'font-size="{FONT_SIZE}" font-weight="bold" '
                     f'text-anchor="middle" fill="{name_colour}">'
                     f'{escape(box.label)}</text>')
        if note:
            parts.append(f'<text x="{box.x}" y="{box.y + 13}" '
                         f'font-size="{NOTE_SIZE}" text-anchor="middle" '
                         f'fill="{note_colour}">{escape(note)}</text>')

    parts.append('</svg>')
    return "\n".join(parts) + "\n"


def main():
    parser = argparse.ArgumentParser(description=__doc__.splitlines()[0])
    parser.add_argument("-o", "--output", required=True)
    parser.add_argument("file")
    args = parser.parse_args()

    with open(args.file, encoding="utf-8") as f:
        spec = yaml.safe_load(f)
    os.makedirs(args.output, exist_ok=True)
    for variant in spec["variants"]:
        path = os.path.join(args.output, f"processes-{variant}.svg")
        with open(path, "w", encoding="utf-8") as f:
            f.write(draw(spec, variant))


if __name__ == "__main__":
    main()
