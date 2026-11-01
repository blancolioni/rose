#!/bin/sh
ARCH=i686
OS=rose
TARGET=$ARCH-$OS
DIRS="src/adainclude src/arch/$ARCH ../../src/base ../../src/library/capabilities ../../src/library/kernelapi/system-calls ../../src/library/kernelapi/system-calls-user ../../src/library/kernelapi/arch/$ARCH"

# The GNAT units that the RTS takes unchanged from the installed GCC: those
# listed in gnat-units (#416).

GNAT_INCLUDE=`gcc -print-file-name=adainclude`
mkdir -p build/gnat-src
for file in `grep -v '^#' gnat-units`
do
    cp $GNAT_INCLUDE/$file build/gnat-src/ || exit 1
done
chmod 644 build/gnat-src/*
DIRS="$DIRS build/gnat-src"

# The generated interface packages that the RTS uses, from the user target
# (#538): only their top-level specs, because every unit in the RTS's
# source directories goes into the library. Process needs the specs of
# its parents, and of the interfaces that its operations name. Programs
# built with the RTS put build/interfaces first in their source
# directories, so that they use these specs and not other copies.

IDLE=${IDLE:-idle}
INTERFACES="process heap cap_set segment server cap region stream_reader"
mkdir -p build/idl build/interfaces
IDL_FILES=""
for name in $INTERFACES
do
    IDL_FILES="$IDL_FILES ../../interfaces/$name.idl"
done
$IDLE --target user -I ../../interfaces -o build/idl $IDL_FILES || exit 1
for name in $INTERFACES
do
    cp build/idl/rose-interfaces-$name.ads build/interfaces/ || exit 1
done
DIRS="$DIRS build/interfaces"

for src in $DIRS
do
    for file in $src/*.ad[bs]
    do
	ln -s `pwd`/$file build/$TARGET/adainclude/$(basename $file)
    done
done
