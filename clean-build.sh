#!/bin/sh
make clean
make idl
(cd rts; make)
(cd src/rts; make)
make
make doc
