ARCH=i686
TARGET=$(ARCH)-unknown-rose
ROSE=./build/$(TARGET)
BUILDDIR=./build/$(ARCH)/kernel/obj
MODULEDIR=./build/$(ARCH)/modules
DRIVERDIR=./build/$(ARCH)/drivers
RELOC=$(BUILDDIR)/rose.reloc.o
GCC=gcc
PROJECT=projects/kernel_$(ARCH).gpr
PROJDRIVERS=projects/drivers_$(ARCH).gpr
TOOLS=idl configure-driver

NULLSTREAM=./build/$(TARGET)/rose-drivers-null_stream
#DRIVERS=$(NULLSTREAM)
DRIVERS=command keyboard event-input-stream exec
UTILITIES=echo tests petal
BOOT_MODULES=init console store mem pci ata isofs restore scan partition elf timer caps checkpoint log

rose: config interfaces $(ROSE) $(BOOT_MODULES) $(DRIVERS) $(UTILITIES) exports stripped hdd iso finished

all: rts idl rose doc

rts:
	(cd rts; make)
	(cd src/rts; make)

tools: $(TOOLS)

interfaces:
	(cd src/library/kernelapi/generated; make)

$(PROJECT):
	sed s/ARCH/$(ARCH)/g projects/kernel-template.gpr > $(PROJECT)

$(BOOT_MODULES): %:
	(cd src/servers/$@; make)

$(DRIVERS): %:
	(cd src/drivers/$@; make)
	(cd images/iso/rose/install/drivers; configure-driver ../../../../../src/drivers/$@/$@.driver)

$(UTILITIES): %:
	mkdir -p images/iso/rose/install/bin/
	(cd src/utilities/$@; make)
	(cp src/utilities/$@/build/bin/$@-driver images/iso/rose/install/bin/$@)
	(cd images/iso/rose/install/bin; configure-driver ../../../../../src/utilities/$@/$@.caps)

exports:
	sh ./scripts/export-elf-trace

stripped:
	sh ./scripts/prepare-boot-executables

hdd:
	sh ./scripts/rose-hdd-install

iso:
	sh ./scripts/rose-iso-install

floppy:
	sh ./scripts/rose-floppy-install

finished:
	@echo Build finished at `date`

doc:
	(cd doc; make)

$(ROSE): $(RELOC)

$(RELOC): kernel boot

kernel:
	gnatmake -Pprojects/kernel_$(ARCH)

boot:
	(cd src/asm/$(ARCH); make)

idl:
	(cd src/tools/idl; make)
	(cd src/library/kernelapi/generated; make)

configure-driver:
	(cd src/tools/configure-driver; make)

config:
	mkdir -p $(BUILDDIR)
	mkdir -p $(MODULEDIR)
	mkdir -p $(DRIVERDIR)
	mkdir -p ./images/iso/rose/install/drivers


clean:
	(cd src/library/kernelapi/generated; make clean)
	(cd rts; make clean)
	(cd src/rts; make clean)
	(cd doc; make clean)
	rm -rf build/*
	rm -rf `find src -name build -print`
	rm -f `find src -name "*.o" -print`
	rm -f images/rose-boot-floppy.img
	rm -f $(ROSE)
	rm -f `find . -name "*~" -print`
	rm -f `find . -name "b~*" -print`
	rm -f src/tools/idl/idl.gpr
	rm -f src/tools/configure-driver/configure.gpr
	rm -f src/rts/support/librose-util.a

