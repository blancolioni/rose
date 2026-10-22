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

With --check, diagrams that have a 'check' section are compared with
the layout GNAT actually gives the named Ada records:

    check:
      records:
        - record: Rose.Capabilities.Layout.Capability_Header
          offset: 0            # bit offset in the diagram, default 0
          fields:
            Type: Cap_Type     # diagram label: record component
      values:
        "0100": Rose.Capabilities.Layout.Endpoint_Cap

Each 'values' entry says that the constant field with that label holds
the representation of the given enumeration literal.

Usage: bitlayout.py -o OUTDIR [--check -I DIR... --ada-arg=ARG...]
                    FILE.yaml...
"""

import argparse
import json
import os
import re
import subprocess
import sys
import tempfile
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


def field_positions(word, rows):
    """Map each label to the (first bit, size) of every field with it."""
    positions = {}
    base = 0
    for row in rows:
        if row[0] == "fields":
            for lo, hi, label, _ in row[1]:
                positions.setdefault(label, []).append(
                    (base + lo, hi - lo + 1))
            base += word
        else:
            _, words, label = row
            positions.setdefault(label, []).append((base, words * word))
            base += words * word
    return positions


ADA_NAME = r"[A-Za-z]\w*(\.[A-Za-z]\w*)*"


def ada_name(where, value):
    if not isinstance(value, str) or not re.fullmatch(ADA_NAME, value):
        raise LayoutError(f"{where}: bad Ada name {value!r}")
    return value


def parse_check(name, value):
    """Return ([(record, offset, {label: component})], [(label, literal)])."""
    if not isinstance(value, dict) or not set(value) <= {"records",
                                                         "values"}:
        raise LayoutError(f"{name}: check must have records and/or values")
    records = []
    for index, item in enumerate(value.get("records", [])):
        where = f"{name}, check record {index + 1}"
        if not isinstance(item, dict) \
                or not isinstance(item.get("fields"), dict):
            raise LayoutError(f"{where}: expected record and fields")
        offset = item.get("offset", 0)
        if not isinstance(offset, int):
            raise LayoutError(f"{where}: bad offset {offset!r}")
        fields = {str(label): ada_name(where, component)
                  for label, component in item["fields"].items()}
        records.append((ada_name(where, item.get("record")), offset, fields))
    values = []
    for label, literal in value.get("values", {}).items():
        if not isinstance(label, str) or not re.fullmatch(r"[01]+", label):
            raise LayoutError(f"{name}: value label {label!r} must be "
                              f"a quoted binary string")
        values.append((label, ada_name(name, literal)))
    return records, values


def check_layouts(path, word, checks, args):
    """Compare each diagram's checked fields with GNAT's record layout.

    The records are compiled as derived types in a throwaway procedure,
    since GNAT only reports layouts for units it generates code for.
    """
    records = sorted({record for _, _, (recs, _) in checks
                      for record, _, _ in recs})
    literals = [(name, label, literal)
                for name, _, (_, values) in checks
                for label, literal in values]
    packages = sorted({n.rsplit(".", 1)[0] for n in records}
                      | {lit.rsplit(".", 1)[0] for _, _, lit in literals})
    source = [f"with {package};" for package in packages]
    source.append("procedure Layout_Check is")
    for index, record in enumerate(records):
        source.append(f"   type T{index} is new {record};")
    for index, (_, label, literal) in enumerate(literals):
        source.append(f"   pragma Compile_Time_Warning\n"
                      f"     ({literal}'Enum_Rep /= 2#{label}#,"
                      f" \"bitlayout:{index}\");")
    source += ["begin", "   null;", "end Layout_Check;", ""]

    with tempfile.TemporaryDirectory() as tmp:
        with open(os.path.join(tmp, "layout_check.adb"), "w") as f:
            f.write("\n".join(source))
        result = subprocess.run(
            ["gcc", "-c", "-gnatR3j", *args.ada_arg,
             *(f"-I{os.path.abspath(d)}" for d in args.include),
             "layout_check.adb"],
            cwd=tmp, capture_output=True, text=True)
    if result.returncode != 0:
        raise LayoutError(f"{path}: compiling the layout check failed:\n"
                          f"{result.stderr}")

    layouts = {}
    for item in json.loads(result.stdout):
        components = {}
        for c in item.get("record", []):
            if not all(isinstance(c.get(k), int)
                       for k in ("Position", "First_Bit", "Size")):
                raise LayoutError(f"{path}: no static layout for "
                                  f"{item['name']}.{c['name']}")
            components[c["name"].lower()] = \
                (c["Position"] * 8 + c["First_Bit"], c["Size"])
        layouts[item["name"].lower()] = components

    errors = []
    for name, rows, (recs, _) in checks:
        positions = field_positions(word, rows)
        for record, offset, fields in recs:
            index = records.index(record)
            components = layouts[f"layout_check.t{index}"]
            for label, component in fields.items():
                found = positions.get(label, [])
                if len(found) != 1:
                    errors.append(f"{name}: expected one field labelled "
                                  f"'{label}', found {len(found)}")
                    continue
                if component.lower() not in components:
                    errors.append(f"{name}: {record} has no component "
                                  f"{component}")
                    continue
                first, size = components[component.lower()]
                first += offset
                if found[0] != (first, size):
                    lo, bits = found[0]
                    errors.append(
                        f"{name}: '{label}' is bits {lo}..{lo + bits - 1}"
                        f", but {component} is bits"
                        f" {first}..{first + size - 1}")
    for index, (name, label, literal) in enumerate(literals):
        if re.search(rf"\bbitlayout:{index}\b", result.stderr):
            errors.append(f"{name}: '{label}' is not the representation "
                          f"of {literal}")
    return errors


def process(path, args):
    with open(path, encoding="utf-8") as f:
        data = yaml.safe_load(f)
    if not isinstance(data, dict) or not isinstance(data.get("diagrams"),
                                                     dict):
        raise LayoutError(f"{path}: expected a 'diagrams' mapping")
    word = data.get("word", 32)
    checks = []
    for name, value in data["diagrams"].items():
        if not re.fullmatch(r"[a-z0-9][a-z0-9-]*", str(name)):
            raise LayoutError(f"{path}: bad diagram name {name!r}")
        rows = parse_diagram(name, word, value)
        if args.check and "check" in value:
            checks.append((name, rows, parse_check(name, value["check"])))
        svg = render(name, value.get("title", name), word, rows)
        with open(os.path.join(args.output, f"{name}.svg"), "w",
                  encoding="utf-8", newline="\n") as f:
            f.write(svg)
    return check_layouts(path, word, checks, args) if checks else []


def main():
    parser = argparse.ArgumentParser(description=__doc__.splitlines()[0])
    parser.add_argument("-o", "--output", required=True,
                        help="directory for the SVG files")
    parser.add_argument("--check", action="store_true",
                        help="check diagrams against the Ada records")
    parser.add_argument("-I", dest="include", action="append", default=[],
                        help="Ada source directory for --check")
    parser.add_argument("--ada-arg", action="append", default=[],
                        help="extra compiler argument for --check")
    parser.add_argument("files", nargs="+", help="layout YAML files")
    args = parser.parse_args()
    os.makedirs(args.output, exist_ok=True)
    errors = []
    try:
        for path in args.files:
            errors += process(path, args)
    except LayoutError as e:
        print(f"bitlayout: {e}", file=sys.stderr)
        sys.exit(1)
    for error in errors:
        print(f"bitlayout: {error}", file=sys.stderr)
    if errors:
        sys.exit(1)


if __name__ == "__main__":
    main()
