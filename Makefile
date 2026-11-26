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

DRIVERS=command keyboard exec file directory null_stream vga-console clock caps screen surface-console process_probe
UTILITIES=echo list cat touch mkdir rename remove rmdir copy clear date set_date uptime user auth rights login supervisor run_self_tests hold_derived hold_file hold_home swap_probe page_probe hold_pages hold_transient replaceable_test petal argv keytest kilo

# The boot modules are the ones boot.yaml lists (#374), which is also where
# the GRUB menus' module2 lines come from.
BOOT_MODULES := $(shell python3 scripts/make-init --list-modules src/servers/init/boot.yaml)
ifeq ($(BOOT_MODULES),)
$(error no boot modules: scripts/make-init --list-modules printed nothing)
endif

# The default target builds a bootable ISO.  The hard disk image (hdd), the
# book (doc) and publishing (publish-iso) are separate; all does the lot.

rose: config interfaces $(ROSE) check-targets check-endpoint-ids install-list programs exports stripped bootdisk iso finished

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

.PHONY: all rose programs check-env check-targets check-endpoint-ids check-install-list install-list host-commands test-host test test-screen test-restore restore-fixture test-drawing test-store prove rts interfaces idl config kernel boot exports stripped \
    bootdisk hdd iso finished doc clean publish-iso version \
    $(BOOT_MODULES) $(DRIVERS) $(UTILITIES)

# Check the tools before building anything; clean-build.sh does this first.
check-env:
	sh ./scripts/check-env

# Check that every program built with the userland run-time system uses the
# user IDL target (#490), before building them.
check-targets:
	sh ./scripts/check-targets

# Check that no endpoint id has changed (#736): a changed id breaks the
# restore of a checkpoint taken before it. scripts/check-endpoint-ids
# --update writes the list after a deliberate change. Within a release
# series, every id of the series' first tag must still be there (#827).
check-endpoint-ids:
	sh ./scripts/check-endpoint-ids

# The install list (#1100): the programs an install puts on a system, in
# order, which restore-install reads from /rose/install/list. It is checked
# against DRIVERS and UTILITIES, then copied to build/install, which
# scripts/rose-iso-install puts on the ISO. The copy is only made again
# when one of these changes, so a line added to build/install/list by
# hand, for a program from outside Rose, stays until then.
INSTALL_LIST=src/servers/restore/install.list
INSTALL_FLAGS=src/servers/restore/src/restore-installer.ads

check-install-list:
	python3 ./scripts/check-install-list $(INSTALL_LIST) $(INSTALL_FLAGS) \
	    "$(DRIVERS)" "$(UTILITIES)"

install-list: build/install/list

build/install/list: $(INSTALL_LIST) $(INSTALL_FLAGS) scripts/check-install-list Makefile
	$(MAKE) check-install-list
	mkdir -p build/install
	cp $(INSTALL_LIST) $@

# Boot the ISO under QEMU, run the self tests, and check the checkpoint log.
# This doesn't build anything, so run "make" first.  It takes about ten
# minutes.
test:
	sh ./scripts/test-boot

# The restore test (#737): this build has to restore the checkpoint of a
# disk installed by the release where its series froze, X.Y.0 (0.4.1 for
# 0.4, scripts/frozen-release), whose fixture is on the GitHub release.
# Until that release is out there is no fixture, and it passes with a
# message.  CI runs it.
test-restore:
	sh ./scripts/test-restore-fixture

# Three boots from one install, each restoring the checkpoint of the boot
# before, with hold_pages' 48 MiB checked each time (#975).  It needs
# images/rose.iso, so run "make" first.  It takes about fifteen minutes,
# and isn't part of CI.
test-restores:
	sh ./scripts/test-restores

# The update test (#830): this build's install media has to update the
# same fixture, keeping its checkpoint and replacing its replaceable
# servers, and then hd0 has to boot the new set and Previous.  It needs
# images/rose.iso, so run "make" first, and passes with a message while
# there is no fixture.  It takes about fifteen minutes.  CI runs it.
test-update:
	sh ./scripts/test-update-fixture

# The fixture for test-restore: install images/rose.iso, edit a file, and
# keep the disk, compressed, in images/fixtures.  At a release, upload it
# to the release (scripts/make-restore-fixture).
restore-fixture:
	sh ./scripts/make-restore-fixture

# The same with a framebuffer: the console on the screen's surface, and
# the screen driver's keys and redraw after a restore (#582, #653).  It
# isn't part of "make test", or of CI.
test-screen:
	sh ./scripts/test-screen

# The tests of the drawing library and its text (#579, #580), built and
# run on the host, in a few seconds.  CI runs them.
test-drawing:
	cd src/library/drawing/tests && gprbuild -q -P drawing_tests.gpr \
	    && ./build/test_drawing

# The tests of the store's block map and free map (ADR 0038, #504), built
# and run on the host, in a few seconds.  CI runs them.
# Commands built for the host's own Ada run-time, to test them without
# booting Rose (#1101): each with "make host" in its directory (mk/host.mk),
# and idle --target host. They need idle, and nothing else that Rose
# builds. test-host runs them (scripts/test-host).
HOST_COMMANDS=echo cat list touch copy mkdir remove rename date

host-commands:
	for command in $(HOST_COMMANDS); do \
	    $(MAKE) -C src/utilities/$$command host || exit 1; \
	done

test-host: host-commands
	sh ./scripts/test-host

test-store:
	cd src/servers/store/tests && gprbuild -q -P page_maps_tests.gpr \
	    && ./build/test_page_maps

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

# Prove the packages that are in SPARK (projects/spark.gpr).  gnatprove comes
# from Alire, which puts it in ~/.alire/bin, so GNATPROVE can name it.  The
# level is 1, and the book says which packages are proved at it.
GNATPROVE ?= gnatprove
PROVE_LEVEL ?= 1

prove: $(USER_RTS_LIB)
	ROSE_USER_RTS=$(CURDIR)/$(USER_RTS) $(GNATPROVE) -P projects/spark.gpr --mode=all --level=$(PROVE_LEVEL) -j0

interfaces: idl

$(PROJECT):
	sed s/ARCH/$(ARCH)/g projects/kernel-template.gpr > $(PROJECT)

$(BOOT_MODULES): %:
	$(MAKE) -C src/servers/$@

$(DRIVERS): %:
	$(MAKE) -C src/drivers/$@
	cp src/drivers/$@/$@.driver build/install/drivers/$@.caps

# The installed copy is stripped, so that every program fits in exec's
# storage region; the build directory keeps the one with debug info.

$(UTILITIES): %:
	mkdir -p build/install/bin/
	$(MAKE) -C src/utilities/$@
	strip -o build/install/bin/$@ src/utilities/$@/build/bin/$@-driver
	cp src/utilities/$@/$@.caps build/install/bin/$@.caps

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

$(BOOT_MODULES): config interfaces version $(KERNEL_RTS_LIB) $(ROSE)
$(DRIVERS) $(UTILITIES): config interfaces version $(USER_RTS_LIB)
kernel: config interfaces version
boot: kernel
exports stripped: programs
bootdisk: stripped
iso: bootdisk
finished: iso exports

# The generated interface packages that the kernel uses, for their
# endpoint ids, and their groups' packages (ADR 0034). Only these are
# linked into the kernel's sources.

KERNEL_INTERFACES=checkpoints checkpoints-kernel_log processes \
    processes-memory processes-process processes-restorable \
    processes-signalled

# Links left from an earlier list, such as before the interfaces were
# grouped, would name files that no longer exist, so they go first.

kernel:
	mkdir -p build/$(ARCH)/kernel/interfaces
	rm -f build/$(ARCH)/kernel/interfaces/*.ads
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
	mkdir -p ./build/install/drivers

# Rose.Build, the build id in the boot banners, and Rose.Interface_Level,
# the interfaces that a checkpoint records (#375), in build/version.  They
# run on every build, and only rewrite a file when it changes.
version:
	sh ./scripts/make-build-id
	sh ./scripts/make-interface-level

# What the build makes, and nothing else: the generated interfaces, the run-time
# systems (which clean themselves), the programs' build directories, the
# images, and the logs.  What is in git stays, such as logs/rose-log-files.txt,
# and so does a disk image (rose-hd0.img): that is data.
clean:
	$(MAKE) -C src/library/kernelapi/generated clean
	$(MAKE) -C rts/kernel clean
	$(MAKE) -C rts/user clean
	$(MAKE) -C doc clean
	rm -rf build/*
	rm -rf src/servers/*/build src/drivers/*/build src/utilities/*/build
	rm -rf src/library/host/build
	rm -rf images/tmp images/drivers images/rose.iso
	rm -f logs/rose-objdump.txt logs/rose-*-objdump.txt logs/rose-*-elf.txt logs/rose-*.sha1 logs/rose-iso.log
	find src rts projects -name "b~*" -delete

