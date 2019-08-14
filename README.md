# rose
Persistent capability-based kernel

## Requirements

The build process requires [alix](https://github.com/blancolioni/alix) to generate source files from .idl specifications.

## Build

```
cd rose
(cd rts ; make)
make idl
make
```

A bootable iso is created in the images directory.

## Status

Rose boots, launches various services, and
reformats that hard drive.  It is therefore not what 
you would call ready for production.
