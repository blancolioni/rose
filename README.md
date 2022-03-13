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

<img width="363" alt="rose-boot" src="https://user-images.githubusercontent.com/53253609/158062282-6eb41c80-2752-47f1-9f02-e131b956bf00.png">

Petal is the rose shell, obviously.
