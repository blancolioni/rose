ARCH=i686
TARGET=$(ARCH)-unknown-rose
ROSE=./build/$(TARGET)/rose
BUILDDIR=./build/$(ARCH)/kernel/obj
MODULEDIR=./build/$(ARCH)/modules
RELOC=$(BUILDDIR)/rose.reloc.o
GCC=gcc
PROJECT=projects/kernel_$(ARCH).gpr
PROJDRIVERS=projects/drivers_$(ARCH).gpr

NULLSTREAM=./build/$(TARGET)/rose-drivers-null_stream
#DRIVERS=$(NULLSTREAM)
DRIVERS=
BOOT_MODULES=init console mem pci ata restore
#BOOT_MODULES=init console mem pci ram_disk ata restore

#all: $(PROJECT) $(ROSE) $(PROJDRIVERS) $(DRIVERS)
all: config $(ROSE) $(DRIVERS) $(BOOT_MODULES) exports stripped hdd floppy iso finished

rts:
	(cd rts; make)

$(PROJECT):
	sed s/ARCH/$(ARCH)/g projects/kernel-template.gpr > $(PROJECT)

$(PROJDRIVERS):
	sed s/ARCH/$(ARCH)/g projects/drivers-template.gpr > $(PROJDRIVERS)

$(NULLSTREAM): $(PROJDRIVERS)
	gnatmake -P$(PROJDRIVERS)

init:
	(cd src/servers/init; make)

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

config:
	mkdir -p $(BUILDDIR)
	mkdir -p $(MODULEDIR)

clean:
	rm -rf build/*
	rm -f `find src -name "*.o" -print`
	rm -f images/rose-boot-floppy.img
	rm -f $(ROSE)
	rm -f `find . -name "*~" -print`
	rm -f `find . -name "b~*" -print`
