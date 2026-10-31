#!/bin/sh
# Build from scratch.  --book builds the book as well, which needs
# asciidoctor; see the README.
set -e
book=no
for arg in "$@"
do
    case $arg in
        --book) book=yes ;;
        *) echo "usage: clean-build.sh [--book]" >&2; exit 1 ;;
    esac
done
make clean
make idl
(cd rts; make)
(cd src/rts; make)
make
if [ $book = yes ]; then
    (cd doc/rose-book/; make adoc)
fi
