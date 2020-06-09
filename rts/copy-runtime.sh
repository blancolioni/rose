#!/bin/sh
GCCVERSION=10
for f in "ada.ads" "a-unccon.ads" "a-uncdea.ads" "gnat.ads" "g-souinf.ads" \
   interfac.ads s-atacco.adb s-atacco.ads s-maccod.ads s-stoele.adb \
   s-stoele.ads i-c.ads s-addima.ads s-addope.ads s-imgint.ads \
   s-unstyp.ads
do
    mkdir -p ./gnat-src
    rm -f ./gnat-src/$f
    cp /usr/lib/gcc/x86_64-redhat-linux/$GCCVERSION/adainclude/$f ./gnat-src
    rm -f ./build/i686/adainclude/$f
    ln -s `pwd`/gnat-src/$f `pwd`/build/i686/adainclude/$f
done
cp arch/i686/system.ads ./build/i686/adainclude
