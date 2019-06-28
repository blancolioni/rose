DIR=$1
NAME=$2
DEST=$3
SRC=$ROOT/src/$DIR/$NAME
MAKEFILE=$SRC/Makefile
GPR=$SRC/$NAME.gpr
GPRBUILD=gprbuild
GPRBUILD_ARGS="--compiler-subst=ada,$TARGET-gcc --RTS=$ROOT/rts/build/$TARGET"

echo -n " $NAME"

rm -rf $SRC/build

cat > $GPR <<EOF
project $NAME is

   for Source_Dirs use 
      ("src",
       "../../base",
       "../../library/capabilities",
       "../../library/kernelapi/console",
       "../../library/rose",
       "../../library/kernelapi/devices",
       "../../library/kernelapi/generated",
       "../../library/kernelapi/system-calls",
       "../../library/kernelapi/arch/i686");

   for Object_Dir use "./build/obj";
   for Exec_Dir use "./build/bin";
   for Main use ("$NAME-driver.adb");

   package Builder is
      for Global_Configuration_Pragmas use "$NAME-pragmas.config";
      for Default_Switches ("ada") use ("-s", "-k", "-g", "-O3", "-C", "-j2");
   end Builder;

   package Compiler is
      for Default_Switches ("ada") use ("-g", "-gnatwael", "-gnatVa", "-gnatyabcefhiklmnoprst", "-gnata", "-gnato", "-gnatp", "-m32", "-gnat12", "-fno-strict-aliasing");
   end Compiler;

   package Binder is
      for Default_Switches ("ada") use ("-E");
   end Binder;

   package Linker is
      for Default_Switches ("ada") use ("-m32", "../../../build/$TARGET/kernel/obj/util.o", "-nostdlib");
   end Linker;

   package Pretty_Printer is
      for Default_Switches ("ada") use ("-M72");
   end Pretty_Printer;

end $NAME;

EOF

(cd $SRC; mkdir -p ./build/obj; mkdir -p ./build/bin; $GPRBUILD -c $GPRBUILD_ARGS $NAME.gpr > /dev/null)

cat > $MAKEFILE <<EOF
TARGET=$TARGET
BUILDDIR=./build/obj
DEFINES=
GCC=\$(TARGET)-gcc
GPRBUILD=gprbuild
GPRBUILD_ARGS=--compiler-subst=ada,\$(GCC) --RTS=$ROOT/rts/build/$TARGET
GCC_OPTS=-std=gnu99 -ffreestanding -g -finline-limit=3000 -fno-strict-aliasing -Wall -Winline -Wno-format -Wno-char-subscripts -Werror -m32 -nostdinc -nostdlib
UTIL=$ROOT/src/library/util.c
CFILES=\$(UTIL)
OBJECTS=`echo -n $SRC/build/obj/*.o`
all: $NAME
	\$(GCC) -Wl,-Tlinker.ld  \$(GCC_OPTS) -nostdlib -o ../../../build/\$(TARGET)/$DEST/$NAME \$(OBJECTS) $ROOT/build/\$(TARGET)/kernel/obj/util.o $ROOT/rts/build/$TARGET/adalib/libgnat.a -g

$NAME:
	mkdir -p build/obj
	mkdir -p build/bin
	\$(GPRBUILD) \$(GPRBUILD_ARGS)

\$(BUILDDIR)/ada_rts.o: ../../library/asm/i686/ada_rts.s
	\$(GCC) -c -m32 ../../library/asm/i686/ada_rts.s -o \$(BUILDDIR)/ada_rts.o

EOF

rm -rf $SRC/build
