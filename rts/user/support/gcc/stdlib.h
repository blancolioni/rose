/* stdlib.h: the little of the C library that raise-gcc.c includes, for
   Rose's freestanding build of it (#170).  */

#ifndef ROSE_STDLIB_H
#define ROSE_STDLIB_H

#include <stddef.h>

extern void abort (void) __attribute__ ((noreturn));

#endif
