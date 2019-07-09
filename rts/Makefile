ARCH=i686
BUILD=./build/$(ARCH)
ADAINCLUDE=$(BUILD)/adainclude
ADALIB=$(BUILD)/adalib
OBJ=$(BUILD)/obj

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
	gprbuild -v rts.gpr
