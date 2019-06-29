TARGET=i686-elf
BUILD=./build/$(TARGET)
ADAINCLUDE=$(BUILD)/adainclude
ADALIB=$(BUILD)/adalib
OBJ=$(BUILD)/obj
GPRBUILD=gprbuild -v --compiler-subst=ada,$(TARGET)-gcc
all: clean config copy rts

clean:
	rm -rf build
	rm -rf gnat-src

config:
	mkdir -p $(ADAINCLUDE)
	mkdir -p $(ADALIB)
	mkdir -p $(OBJ)

copy:
	sh ./copy-runtime.sh

rts:
	$(GPRBUILD) rts.gpr
