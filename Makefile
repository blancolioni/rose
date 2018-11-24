ARCH=i686
TARGET=$(ARCH)-unknown-rose
ROSE=./build/$(TARGET)/rose
BUILDDIR=./build/$(ARCH)/kernel/obj
MODULEDIR=./build/$(ARCH)/modules
RELOC=$(BUILDDIR)/rose.reloc.o
GCC=gcc
PROJECT=projects/kernel_$(ARCH).gpr
PROJDRIVERS=projects/drivers_$(ARCH).gpr
TOOLS=idl

NULLSTREAM=./build/$(TARGET)/rose-drivers-null_stream
#DRIVERS=$(NULLSTREAM)
DRIVERS=
BOOT_MODULES=init console store mem pci ata isofs restore scan

#all: $(PROJECT) $(ROSE) $(PROJDRIVERS) $(DRIVERS)
all: config interfaces $(ROSE) $(DRIVERS) $(BOOT_MODULES) exports stripped hdd floppy iso finished

rts:
	(cd rts; make)

tools: $(TOOLS)
	
interfaces:
	(cd src/library/kernelapi/generated; make)

$(PROJECT):
	sed s/ARCH/$(ARCH)/g projects/kernel-template.gpr > $(PROJECT)

$(PROJDRIVERS):
	sed s/ARCH/$(ARCH)/g projects/drivers-template.gpr > $(PROJDRIVERS)

$(NULLSTREAM): $(PROJDRIVERS)
	gnatmake -P$(PROJDRIVERS)

init:
	(cd src/servers/init; make)

store:
	(cd src/servers/store; make)

mem:
	(cd src/servers/mem; make)

console:
	(cd src/servers/console; make)

pci:
	(cd src/servers/pci; make)

ram_disk:
	(cd src/servers/ram_disk; make)

ata:
	(cd src/servers/ata; make)

isofs:
	(cd src/servers/isofs; make)

scan:
	(cd src/servers/scan; make)

restore:
	(cd src/servers/restore; make)

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

config:
	mkdir -p $(BUILDDIR)
	mkdir -p $(MODULEDIR)

clean:
	(cd src/library/kernelapi/generated; make clean)
	rm -rf build/*
	rm -f `find src -name "*.o" -print`
	rm -f images/rose-boot-floppy.img
	rm -f $(ROSE)
	rm -f `find . -name "*~" -print`
	rm -f `find . -name "b~*" -print`
