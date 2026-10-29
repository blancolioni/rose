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

NULLSTREAM=./build/$(TARGET)/rose-drivers-null_stream
#DRIVERS=$(NULLSTREAM)
DRIVERS=command keyboard exec file directory vga-console
UTILITIES=echo list cat touch mkdir clear user auth run_self_tests petal
BOOT_MODULES=init console store mem pci ata isofs restore scan partition elf timer caps checkpoint log

rose: config interfaces $(ROSE) $(BOOT_MODULES) $(DRIVERS) $(UTILITIES) exports stripped bootdisk hdd iso finished

all: idl rts rose doc

rts:
	(cd rts; make)
	(cd src/rts; make)

interfaces:
	(cd src/library/kernelapi/generated; make)

$(PROJECT):
	sed s/ARCH/$(ARCH)/g projects/kernel-template.gpr > $(PROJECT)

$(BOOT_MODULES): %:
	(cd src/servers/$@; make)

$(DRIVERS): %:
	(cd src/drivers/$@; make)
	cp src/drivers/$@/$@.driver images/iso/rose/install/drivers/$@.caps

# The installed copy is stripped, so that every program fits in exec's
# storage region; the build directory keeps the one with debug info.

$(UTILITIES): %:
	mkdir -p images/iso/rose/install/bin/
	(cd src/utilities/$@; make)
	strip -o images/iso/rose/install/bin/$@ src/utilities/$@/build/bin/$@-driver
	cp src/utilities/$@/$@.caps images/iso/rose/install/bin/$@.caps

exports:
	sh ./scripts/export-elf-trace

stripped:
	sh ./scripts/prepare-boot-executables

# GRUB and the boot partition for hd0, which the installer writes (#322)
bootdisk:
	sh ./scripts/make-boot-disk

hdd:
	sh ./scripts/rose-hdd-install

iso:
	sh ./scripts/rose-iso-install

# Copy the ISO to the rose-os.org web root.  This doesn't build anything,
# so run "make" first.  The copy goes to a temporary name and is renamed
# into place, so a download never gets a half-written ISO.  Its version,
# from Rose.Version, goes in rose.iso.version for the landing page.
ISO_PUBLISH_DIR ?= /var/www/rose-os.org/html

publish-iso:
	test -f images/rose.iso
	cp images/rose.iso $(ISO_PUBLISH_DIR)/.rose.iso.new
	mv -f $(ISO_PUBLISH_DIR)/.rose.iso.new $(ISO_PUBLISH_DIR)/rose.iso
	sed -n 's/.*Version_String *: constant String := "\(.*\)";/\1/p' \
	  src/base/rose-version.ads > $(ISO_PUBLISH_DIR)/rose.iso.version

.PHONY: publish-iso

floppy:
	sh ./scripts/rose-floppy-install

finished:
	@echo Build finished at `date`

doc:
	(cd doc; make)

$(ROSE): $(RELOC)

$(RELOC): kernel boot

# The generated interface packages that the kernel uses, for their
# endpoint ids. Only these are linked into the kernel's sources.

KERNEL_INTERFACES=kernel_log memory restorable

kernel:
	mkdir -p build/$(ARCH)/kernel/interfaces
	for name in $(KERNEL_INTERFACES); do \
	  ln -sf $(CURDIR)/src/library/kernelapi/generated/rose-interfaces-$$name.ads build/$(ARCH)/kernel/interfaces/; \
	done
	gnatmake -Pprojects/kernel_$(ARCH)

boot:
	(cd src/asm/$(ARCH); make)

idl:
	(cd src/library/kernelapi/generated; make)

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
	rm -f src/rts/support/librose-util.a

