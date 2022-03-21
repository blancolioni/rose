#!/bin/sh
make clean
(cd rts; make)
(cd src/rts; make)
make idl
make
make doc
