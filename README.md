# rose
Persistent capability-based kernel

## Requirements

The build process requires [alix](https://github.com/blancolioni/alix) to generate source files from .idl specifications.

On Fedora, you'll need the following packages: qemu-img gcc-gnat gprbuild make xorriso 

## Build

```
git clone https://github.com/blancolioni/rose.git
cd rose
./clean-build.sh
```

A bootable iso is created in the images directory.

## Status

Rose boots, launches various services, and
reformats that hard drive.  It is therefore not what 
you would call ready for production.
