ARCH=i686
BUILD=./build/$(ARCH)
ADAINCLUDE=$(BUILD)/adainclude
ADALIB=$(BUILD)/adalib
OBJ=$(BUILD)/obj

all: clean config copy rts

clean:
	rm -rf build

config:
	mkdir -p $(ADAINCLUDE)
	mkdir -p $(ADALIB)
	mkdir -p $(OBJ)

copy:
	cp -f src/adainclude/* $(ADAINCLUDE)
	cp -f src/arch/$(ARCH)/* $(ADAINCLUDE)

rts:
	gprbuild -v rts.gpr
