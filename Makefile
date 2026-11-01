ROOT=.
PARALLEL=yes
include $(ROOT)/mk/common.mk

TARGET=$(ARCH)-unknown-rose
ROSE=./build/$(TARGET)
BUILDDIR=./build/$(ARCH)/kernel/obj
MODULEDIR=./build/$(ARCH)/modules
DRIVERDIR=./build/$(ARCH)/drivers
RELOC=$(BUILDDIR)/rose.reloc.o
PROJECT=projects/kernel_$(ARCH).gpr
PROJDRIVERS=projects/drivers_$(ARCH).gpr

NULLSTREAM=./build/$(TARGET)/rose-drivers-null_stream
#DRIVERS=$(NULLSTREAM)
DRIVERS=command keyboard exec file directory vga-console clock
UTILITIES=echo list cat touch mkdir rename clear date set_date uptime user auth rights login run_self_tests petal

# The boot modules are the ones boot.yaml lists (#374), which is also where
# the GRUB menus' module2 lines come from.
BOOT_MODULES := $(shell python3 scripts/make-init --list-modules src/servers/init/boot.yaml)
ifeq ($(BOOT_MODULES),)
$(error no boot modules: scripts/make-init --list-modules printed nothing)
endif

# The default target builds a bootable ISO.  The hard disk image (hdd), the
# book (doc) and publishing (publish-iso) are separate; all does the lot.

rose: config interfaces $(ROSE) programs exports stripped bootdisk iso finished

programs: $(BOOT_MODULES) $(DRIVERS) $(UTILITIES)

all:
	$(MAKE) idl
	$(MAKE) rts
	$(MAKE) rose
	$(MAKE) hdd
	$(MAKE) doc

# None of these targets is a file.  Three of them (rts, interfaces and doc)
# are also directory names, so without this make thinks they are up to date
# and skips their recipes.

.PHONY: all rose programs check-env test rts interfaces idl config kernel boot exports stripped \
    bootdisk hdd iso finished doc clean publish-iso \
    $(BOOT_MODULES) $(DRIVERS) $(UTILITIES)

# Check the tools before building anything; clean-build.sh does this first.
check-env:
	sh ./scripts/check-env

# Boot the ISO under QEMU, run the self tests, and check the checkpoint log.
# This doesn't build anything, so run "make" first.  It takes about ten
# minutes.
test:
	sh ./scripts/test-boot

# The run-time systems are built when they are missing, so make works after
# make clean.  They aren't rebuilt when their sources change: "make rts"
# does that.

KERNEL_RTS_LIB=$(KERNEL_RTS)/adalib/libgnat.a
USER_RTS_LIB=$(USER_RTS)/adalib/libgnat.a

$(KERNEL_RTS_LIB):
	$(MAKE) -C rts/kernel

$(USER_RTS_LIB): $(KERNEL_RTS_LIB)
	$(MAKE) -C rts/user

rts:
	$(MAKE) -C rts/kernel
	$(MAKE) -C rts/user

interfaces: idl

$(PROJECT):
	sed s/ARCH/$(ARCH)/g projects/kernel-template.gpr > $(PROJECT)

$(BOOT_MODULES): %:
	$(MAKE) -C src/servers/$@

$(DRIVERS): %:
	$(MAKE) -C src/drivers/$@
	cp src/drivers/$@/$@.driver images/iso/rose/install/drivers/$@.caps

# The installed copy is stripped, so that every program fits in exec's
# storage region; the build directory keeps the one with debug info.

$(UTILITIES): %:
	mkdir -p images/iso/rose/install/bin/
	$(MAKE) -C src/utilities/$@
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
	test -d $(ISO_PUBLISH_DIR)
	cp images/rose.iso $(ISO_PUBLISH_DIR)/.rose.iso.new
	mv -f $(ISO_PUBLISH_DIR)/.rose.iso.new $(ISO_PUBLISH_DIR)/rose.iso
	sed -n 's/.*Version_String *: constant String := "\(.*\)";/\1/p' \
	  src/base/rose-version.ads > $(ISO_PUBLISH_DIR)/rose.iso.version

finished:
	@echo Build finished at `date`

doc:
	$(MAKE) -C doc

$(ROSE): $(RELOC)

$(RELOC): kernel boot

# What has to come before what, for make -j.  The servers link the kernel's
# util.o, which boot builds from the kernel's objects.  Everything that is
# built from IDL needs the generated interfaces.

$(BOOT_MODULES): config interfaces $(KERNEL_RTS_LIB) $(ROSE)
$(DRIVERS) $(UTILITIES): config interfaces $(USER_RTS_LIB)
kernel: config interfaces
boot: kernel
exports stripped: programs
bootdisk: stripped
iso: bootdisk
finished: iso exports

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
	$(MAKE) -C src/asm/$(ARCH)

idl:
	$(MAKE) -C src/library/kernelapi/generated

config:
	mkdir -p $(BUILDDIR)
	mkdir -p $(MODULEDIR)
	mkdir -p $(DRIVERDIR)
	mkdir -p ./images/iso/rose/install/drivers


# What the build makes, and nothing else: the generated interfaces, the run-time
# systems (which clean themselves), the programs' build directories, the
# images, and the logs.  What is in git stays, such as logs/rose-log-files.txt
# and images/iso/boot/grub, and so does a disk image (rose-hd0.img): that is
# data.
clean:
	$(MAKE) -C src/library/kernelapi/generated clean
	$(MAKE) -C rts/kernel clean
	$(MAKE) -C rts/user clean
	$(MAKE) -C doc clean
	rm -rf build/*
	rm -rf src/servers/*/build src/drivers/*/build src/utilities/*/build
	rm -rf images/tmp images/drivers images/iso/rose images/rose.iso
	find images/iso/boot -maxdepth 1 -type f -delete
	rm -f logs/rose-objdump.txt logs/rose-*-objdump.txt logs/rose-*-elf.txt logs/rose-*.sha1 logs/rose-iso.log
	find src rts projects -name "b~*" -delete

