ARCH=i686
IDLE ?= idle
INTERFACES=../../../interfaces

all: date
	cp ./build/bin/date-driver ../../../build/$(ARCH)/drivers/date

# The spec of Rose.Core.Date, its main and its metadata, and the package
# Rose.Core, are generated from IDL (ADR 0002).

date:
	mkdir -p build/obj
	mkdir -p build/bin
	mkdir -p build/generated
	$(IDLE) --target kernel -I $(INTERFACES) -o build/generated \
	    rose-core-date.idl $(INTERFACES)/rose-core.idl
	gprbuild
