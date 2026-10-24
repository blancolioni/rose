#!/bin/sh
ARCH=i686
OS=rose
TARGET=$ARCH-$OS
DIRS="src/adainclude src/arch/$ARCH ../base ../library/capabilities ../library/kernelapi/system-calls ../library/kernelapi/arch/$ARCH"

# The generated interface packages that the RTS uses: only these, because
# every unit in the RTS's source directories goes into the library.

INTERFACES="process cap_set"
mkdir -p build/interfaces
for name in $INTERFACES
do
    ln -sf `pwd`/../library/kernelapi/generated/rose-interfaces-$name.ads build/interfaces/
done
DIRS="$DIRS build/interfaces"

for src in $DIRS
do
    for file in $src/*.ad[bs]
    do
	ln -s `pwd`/$file build/$TARGET/adainclude/$(basename $file)
    done
done
