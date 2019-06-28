ARCH=i686
TARGET=$(ARCH)-elf
ROSE=./build/$(TARGET)/rose
BUILDDIR=./build/$(TARGET)/kernel/obj
MODULEDIR=./build/$(TARGET)/modules
DRIVERDIR=./build/$(TARGET)/drivers
RELOC=$(BUILDDIR)/rose.reloc.o
PROJECT=projects/kernel_$(ARCH).gpr
PROJDRIVERS=projects/drivers_$(TARGET).gpr
TOOLS=idl configure-driver
GCC=$(TARGET)-gcc
GPRBUILD=gprbuild
GPRBUILD_ARGS=--compiler-subst=ada,$(TARGET)-gcc --RTS=`pwd`/rts/build/$(TARGET)

NULLSTREAM=./build/$(TARGET)/rose-drivers-null_stream
#DRIVERS=$(NULLSTREAM)
DRIVERS=keyboard exec
BOOT_MODULES=init console store mem pci ata isofs restore scan partition elf timer caps

all: prepare interfaces $(ROSE) $(BOOT_MODULES) $(DRIVERS) exports stripped hdd floppy iso finished

rts:
	(cd rts; make)

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
	$(GPRBUILD) $(GPRBUILD_ARGS) -Pprojects/kernel_$(ARCH)

boot:
	(cd src/asm/$(ARCH); make)

idl:
	(cd src/tools/idl; make)
	(cd src/library/kernelapi/generated; make)

configure-driver:
	(cd src/tools/configure-driver; make)

prepare:
	echo $(BUILDDIR)
	mkdir -p $(BUILDDIR)
	mkdir -p $(MODULEDIR)
	mkdir -p $(DRIVERDIR)
	mkdir -p ./images/iso/rose/install/drivers


clean:
	(cd src/library/kernelapi/generated; make clean)
	rm -rf build/*
	rm -rf `find src -name build -print`
	rm -f `find src -name "*.o" -print`
	rm -f images/rose-boot-floppy.img
	rm -f $(ROSE)
	rm -f `find . -name "*~" -print`
	rm -f `find . -name "b~*" -print`
	rm -f src/tools/idl/idl.gpr
	rm -f src/tools/configure-driver/configure.gpr
