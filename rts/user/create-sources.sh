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
# its parents, and of the interfaces that its operations name, and
# Rose.Streams those of Reader and Writer (#727). Programs
# built with the RTS get these specs from rose_rts.gpr, and don't keep
# copies of their own (#1080).

IDLE=${IDLE:-idle}
INTERFACES="processes-process processes-heap caps-cap_set processes-segment
    processes-server processes-process_control caps-cap storage-region
    storage-region_access
    streams-reader streams-writer storage-bank storage-dependent
    common-resource processes-signalled processes-restorable"
mkdir -p build/idl build/interfaces
rm -f build/interfaces/*.ads
IDL_FILES=""
for name in $INTERFACES
do
    IDL_FILES="$IDL_FILES ../../interfaces/$name.idl"
done
$IDLE --target user -I ../../interfaces -o build/idl $IDL_FILES || exit 1
# The interfaces' groups (ADR 0034), whose empty packages idle writes
# with them, are copied too.
IDL_GROUPS="caps common processes storage streams"
for name in $IDL_GROUPS $INTERFACES
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

# The RTS's own units: every source in adainclude that is not one of GNAT's
# predefined units, whose file names gprbuild knows. rose_rts.gpr lists
# them, so that the programs' builds find them in a project (#1080).

GNAT_NAMES='^[agis]-|^(ada|gnat|interfac|system)\.ads$'
ls build/$TARGET/adainclude | grep -vE "$GNAT_NAMES" > build/rose-units.lst
