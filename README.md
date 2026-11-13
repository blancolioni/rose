# rose
Persistent capability-based kernel

Website: [rose-os.org](https://rose-os.org)

## Requirements

Rose builds only with gcc 15, and is known to build on Fedora 43.  The
run-time system is built from the GNAT sources, which `rts/kernel/copy-runtime.sh`
and `rts/user/create-sources.sh` copy from the installed compiler, and Rose's
own versions of some of those units are based on gcc 15's.  Other versions of
gcc ship different run-time units, so the build fails with them.  On a newer
Fedora, build in a `fedora:43` container.
`make check-env` checks that everything the build needs is installed, and
`clean-build.sh` runs it first.

The build scripts assume Fedora's file layout.  Install these packages,
which include Fedora's GNAT and gprbuild, and QEMU to boot the result:

```
sudo dnf install git-core make gcc gcc-gnat gprbuild binutils findutils gawk \
    sed python3 python3-pyyaml xorriso grub2-tools grub2-tools-extra \
    grub2-pc-modules mtools qemu-img qemu-system-x86-core curl unzip
```

Install [Alire](https://alire.ada.dev) as well, to build idle (see below).
To test under Bochs too, install bochs and run `sh bochs/run-bochs.sh`.

`clean-build.sh --book` also builds the book, which needs:

```
sudo dnf install asciidoctor rubygem-asciidoctor-pdf
gem install --user-install asciidoctor-epub3
```

## Build

Rose's interfaces are written in IDL and compiled to Ada by
[idle](https://github.com/blancolioni/idle), which isn't on Alire yet, so you
build it first.  idle needs [syn](https://github.com/blancolioni/syn) next to
it.

```
git clone https://github.com/blancolioni/rose.git
cd rose
git clone https://github.com/blancolioni/syn
git clone https://github.com/blancolioni/idle
(cd idle; alr build)
IDLE=$PWD/idle/bin/idle sh ./clean-build.sh
```

A bootable iso is created in the images directory.  `make test` boots it
under QEMU, runs the self tests, and checks the checkpoint log; it needs
`qemu-system-x86`.  `make hdd` creates a blank hard disk image there too, and
`make all` builds everything, including the book.

The scripts in `qemu` and `bochs` use the disk `images/rose-hd0.img`, which
`make hdd` makes (`qemu/init-qemu.sh` makes it if it isn't there), and write
their serial output to `logs`.

`clean-build.sh` runs `make -j` with a job for each processor (set `JOBS` to
change that), and so can you.  `make` builds the run-time systems if they are
missing, but doesn't rebuild them when their sources change: `make rts` does
that.

The drivers and the utilities are built with run-time checks: range, index and
overflow checks, and assertions.  `ROSE_CHECKS=unchecked make` builds them
without any, as they were built before, which makes them 1% to 6% smaller.

## Status

Rose boots, launches various services, and
reformats the hard drive.  It is therefore not what 
you would call ready for production.

## Screenshots

<img src="doc/images/install-scan.png" alt="the installer scanning the disk">

The first boot from the iso.  The bar at the top contains the name of the system, the amount of allocated kernel memory, the total available memory, the currently running process, the number of page faults, and the up time.  The PCI and ATA servers find the disk, and `scan` partitions it.

<img src="doc/images/install-restore.png" alt="restore installing the system image">

`restore` creates the system image on the new store, installing each driver and utility.

<img src="doc/images/install-bootloader.png" alt="restore writing the boot loader">

When it has finished, it writes the boot loader to the disk.

<img src="doc/images/restore-checkpoint.png" alt="booting from the checkpoint">

On the next boot, from the disk, the log is consistent and the system is restored from its checkpoint.

<img src="doc/images/petal.png" alt="logging in and running petal">

After login, a petal shell starts.  I typed a few commands, and received results.  `ls` isn't a command; `list` is.

<img src="doc/images/petal-power-cycle.png" alt="the same screen after a power cycle">

The same screen after a power cycle.  Look at the up time: the machine has just started again, but the session, including the screen, is exactly as it was, because Rose is persistent.
