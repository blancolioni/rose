# rose
Persistent capability-based kernel

## Requirements

The build process requires [alix](https://github.com/blancolioni/alix) to generate source files from .idl specifications.

On Fedora, you'll need the following packages: qemu-img gcc-gnat gprbuild make xorriso 

Building Rose requires gcc 10 or gcc 11.  Earlier versions might also work.

## Build

```
git clone https://github.com/blancolioni/rose.git
cd rose
sh ./clean-build.sh
```

A bootable iso is created in the images directory.

## Status

Rose boots, launches various services, and
reformats that hard drive.  It is therefore not what 
you would call ready for production.

## Screenshots

<img width="361" alt="rose-boot" src="https://user-images.githubusercontent.com/53253609/158064366-a33f056f-e68d-43de-aa72-92bda9d55517.png">

The bar at the top contains a beautiful rose, the amount of allocated kernel memory, the total available memory, the currently running process, the number of page faults, and the up time.

At the end of the boot, a petal shell is launched.  At the prompt I typed a command, and received a result.
