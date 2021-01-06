#!/bin/sh
ARCH=i686
OS=rose
TARGET=$ARCH-$OS
DIRS="src/adainclude src/arch/$ARCH ../base ../library/capabilities ../library/kernelapi/system-calls ../library/kernelapi/arch/$ARCH"

for src in $DIRS
do
    for file in $src/*.ad[bs]
    do
	ln -s `pwd`/$file build/$TARGET/adainclude/$(basename $file)
    done
done
