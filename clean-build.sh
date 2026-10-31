#!/bin/sh
set -e
make clean
make idl
(cd rts; make)
(cd src/rts; make)
make
(cd doc/rose-book/; make adoc)
