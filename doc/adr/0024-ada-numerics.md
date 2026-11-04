# 24. Ada.Numerics in the userland run-time system

- **Status:** Proposed
- **Date:** 2026-11-03
- **Related:** #496 (this decision), #664 (real and complex arrays), #670 (floating-point Image and Value), [ADR 0016](0016-random-numbers.md) (the kernel's random source), [ADR 0020](0020-clock.md) (the clock)

## Context

The userland run-time system had `Ada.Numerics` and the big numbers, but none of the children a program uses: the elementary functions, the complex types, `Float_Random` and `Discrete_Random`. The RTS takes GNAT's units unchanged from the installed GCC where it can (`rts/user/gnat-units`, #416), so the question is what those units stand on.

- **The math functions.** GCC 15's elementary functions and complex types go through `Ada.Numerics.Aux_Generic_Float`, which chooses `Aux_Float`, `Aux_Long_Float` or `Aux_Long_Long_Float`. Each imports the C math functions (`sinf`, `sin`, `sinl`, `powf`, ...) with `Convention => Intrinsic`, and `Aux_Linker_Options` links with `-lm`. GCC expands a few of them inline, such as `sqrt` as `fsqrt`, but calls the rest. Rose has no C library, and the server's 32-bit libm is glibc's.
- **The seed.** `Reset` with no seed asks `System.Random_Seed`, whose GNAT body reads `Ada.Calendar.Clock`. A Rose program can read the clock only if it was given a cap for it (ADR 0020), and otherwise `Clock` raises `Program_Error`.
- **QEMU.** Rose usually runs under QEMU, which emulates the x87 `fsin`, `fcos` and `fptan` in double precision: the low 11 bits of the 80-bit result are zero. The other x87 instructions used here are full precision under QEMU.

## Decision

### 1. Rose provides the math functions, in C, with the x87

`rts/user/support/rose-libm.c` defines the 13 functions that the Aux packages import, in their float, double and long double forms, and `sincos`, which GCC calls in place of a `sin` and a `cos` of the same argument. Each computes in the x87's 80-bit format, and the float and double versions round its result. A Rose `Aux_Linker_Options`, in `src/adainclude`, drops `-lm`. Everything else is GNAT's, unchanged.

- **Square root, arctangent, logarithm and powers** use `fsqrt`, `fpatan`, `fyl2x`, `f2xm1` and `fscale`.
- **`exp` reduces its argument by ln 2 in two parts,** so `f2xm1` sees at most a half, instead of multiplying by log2 e first, which loses low bits as the argument grows.
- **The hyperbolic functions** are built from `exp`, with `expm1` near zero.

A C file in `support` follows the RTS's own `memcpy.c` and `rose-time.c`. Ada bodies for the Aux packages would mean replacing GCC's specs instead of taking them unchanged. A port of a full libm, such as musl's, is more code than Rose needs yet.

### 2. Sine and cosine come from series, not fsin and fcos

The trigonometric functions reduce the argument to `n pi/2 + r`, with `r` at most `pi/4`, by Cody and Waite's method with `pi/2` in three parts, up to 2\*\*30. A larger argument is first reduced by `fprem1` and `2 pi`, which keeps the result in range but not accurate. Sine and cosine of `r` come from their Taylor series to `r**21` and `r**22`, and tangent is their quotient.

`fsin` and `fcos` would be simpler, but QEMU's are double precision, which makes `Long_Long_Float` Sin, Cos and Tan good to only about 1e-16 there. The instructions' own reduction, by a `pi` of 66 bits, also loses most of a double's precision by 10\*\*6. With the series, the results agree with glibc's to within 1 ulp, under QEMU and on hardware alike.

### 3. Reset uses the clock when the program has one

Rose's `System.Random_Seed` body seeds from the clock if `Rose.Clock_Source` has one, which it has once the program has opened the clock, and otherwise from GNAT's default seed, 5489. So without a clock, `Reset` with no seed gives the same sequence on every run: the one that a generator gives when it is never reset.

These generators give deterministic pseudo-random sequences, from seeds that are easy to guess. They are not a source of secrets, and their comments say so. Secrets come from the kernel's random source (ADR 0016).

### 4. The scope

- **In:** `Generic_Elementary_Functions` and its instantiations for `Short_Float`, `Float`, `Long_Float` and `Long_Long_Float`; `Generic_Complex_Types` and `Generic_Complex_Elementary_Functions` and theirs; `Float_Random` and `Discrete_Random`; `System.Exponr`, for `**` of a float by an integer; and, since #664, `Generic_Real_Arrays` and `Generic_Complex_Arrays` and their instantiations, with `System.Generic_Array_Operations`; and, since #670, `'Image` and `'Value` of the float types, of 32-bit and 64-bit fixed point and of decimal types, and `Text_IO.Float_IO` with `Float_Text_IO` and its long variants. `System.Float_Control`'s body is Rose's own and does nothing, since the kernel starts each process with a clean FPU.
- **Out:** `Text_IO.Fixed_IO` and `Decimal_IO`, whose bodies in the installed GCC, built for x86_64, also use the 128-bit image and value units, which i686 can't compile. GCC's sources have the bodies without them, if they are wanted.
- **Targets:** i686, the only one with a userland. On x86_64, the long double functions could stay as they are, but double would be expected to use SSE, so `rose-libm.c` would become one file per architecture.

## Consequences

- **`gnat-units` gains 39 files from GCC,** with the Aux packages, `System.Exponr` and `System.Double_Real`, `System.Random_Numbers`, and the spec of `System.Random_Seed`.
- **`Rose.Clock_Source` has `Has_Source`,** so that the seed can tell whether the clock is open without catching `Program_Error`.
- **The tests are self tests.** `Numerics_Test` checks the functions against known values for each float type to within 8 model epsilons, the complex `Sqrt` and `Exp`, and that two generators given the same seed agree. It runs before the clock tests, and checks that `Reset` without a clock gives the never-reset sequence. `Random_Seed_Test` runs after them, and checks that `Reset` then uses the clock. The order of the suite matters to both. `Arrays_Test` checks a matrix product, `Solve`, `Inverse`, `Determinant` and a symmetric eigensystem, and a complex `Solve` and inner product.
- **Accuracy is bounded where it was cheap to bound,** not everywhere. Trigonometric arguments beyond 2\*\*30 lose accuracy, and long double `pow` multiplies by `log2 x` directly, so it can lose a few bits for very large results. `Float` and `Long_Float` are unaffected by either.
