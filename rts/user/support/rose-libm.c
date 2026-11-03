/* The C math functions that Ada.Numerics.Aux_Float, Aux_Long_Float and
   Aux_Long_Long_Float import (#496). Rose has no libm, so these are
   written with the x87 instructions, in the 80-bit format, and the float
   and double versions round the long double result. They cover the
   arguments that Ada.Numerics.Generic_Elementary_Functions passes: it
   checks the domain of each function before it calls one, and raises
   Argument_Error itself. */

/* x as n pi/2 + r, for the trigonometric functions, which use fsin, fcos
   and fptan on r alone. Those instructions reduce a larger argument
   themselves, but by a pi of 66 bits, which loses most of a double's
   precision by 10**6. Up to 2**30, this is Cody and Waite's reduction,
   with pi/2 in three parts: the first two have 32 bits, so that their
   products with n are exact, and the third has the next 64. Beyond that,
   the argument is reduced by fprem1 and 2 pi, which keeps the result in
   range but not accurate: by 2**64 the argument has no fractional bits
   left. */

static const long double half_pi_1 = 0x6487ed51p-30L;
static const long double half_pi_2 = 0x85a308d3p-65L;
static const long double half_pi_3 = 0x98cc51701b839a25p-132L;
static const long double two_over_pi = 0x517cc1b727220a95p-63L;

static long double
reduce (long double x, int *quadrant)
{
  long double n, two_pi;
  unsigned short status;

  *quadrant = 0;
  if (x - x != 0.0L)
    return x - x;

  if (x <= 0x1p30L && x >= -0x1p30L)
    {
      n = x * two_over_pi;
      __asm__ ("frndint" : "+t" (n));
      *quadrant = (int) n & 3;
      return ((x - n * half_pi_1) - n * half_pi_2) - n * half_pi_3;
    }

  __asm__ ("fldpi; fadd %%st(0), %%st(0)" : "=t" (two_pi));
  do
    __asm__ ("fprem1; fnstsw %%ax"
             : "+t" (x), "=a" (status) : "u" (two_pi));
  while (status & 0x0400);
  return x;
}

static long double
sin_x87 (long double r)
{
  __asm__ ("fsin" : "+t" (r));
  return r;
}

static long double
cos_x87 (long double r)
{
  __asm__ ("fcos" : "+t" (r));
  return r;
}

/* 2**t, with fscale for the integer part and f2xm1 for the rest. Clamping
   t first lets fscale overflow to infinity or underflow to 0 instead of
   working on an infinite t. */

static long double
exp2_x87 (long double t)
{
  long double whole, fraction, result;

  if (t > 16500.0L)
    t = 16500.0L;
  else if (t < -16500.0L)
    t = -16500.0L;

  __asm__ ("frndint" : "=t" (whole) : "0" (t));
  fraction = t - whole;
  __asm__ ("f2xm1" : "+t" (fraction));
  fraction += 1.0L;
  __asm__ ("fscale" : "=t" (result) : "0" (fraction), "u" (whole));
  return result;
}

static long double
log2e (void)
{
  long double result;
  __asm__ ("fldl2e" : "=t" (result));
  return result;
}

/* e**x as 2**n e**r, with x = n ln 2 + r. Multiplying x by log2 e first
   would round away the low bits of the product, which matter more as x
   grows, so this reduces x by ln 2 in two parts, as reduce does by pi/2
   above. Clamping x past the long double range, as exp2_x87 does, keeps n
   within the exact products of the first part. */

static const long double ln_2_1 = 0x162e42ffp-29L;
static const long double ln_2_2 = -0x5c610ca86c3898dp-93L;

static long double
exp_x87 (long double x)
{
  long double n, r, result;

  if (x > 11500.0L)
    x = 11500.0L;
  else if (x < -11500.0L)
    x = -11500.0L;

  n = x * log2e ();
  __asm__ ("frndint" : "+t" (n));
  r = ((x - n * ln_2_1) - n * ln_2_2) * log2e ();
  __asm__ ("f2xm1" : "+t" (r));
  r += 1.0L;
  __asm__ ("fscale" : "=t" (result) : "0" (r), "u" (n));
  return result;
}

/* e**x - 1, accurate near 0, where exp would lose it to the subtraction. */

static long double
expm1_x87 (long double x)
{
  long double t = x * log2e ();

  if (t <= 1.0L && t >= -1.0L)
    {
      __asm__ ("f2xm1" : "+t" (t));
      return t;
    }
  return exp_x87 (x) - 1.0L;
}

/* atan (y / x), in the quadrant of (x, y). */

static long double
atan2_x87 (long double y, long double x)
{
  long double result;
  __asm__ ("fpatan" : "=t" (result) : "0" (x), "u" (y) : "st(1)");
  return result;
}

static long double
abs_x87 (long double x)
{
  return x < 0.0L ? -x : x;
}

long double
sqrtl (long double x)
{
  __asm__ ("fsqrt" : "+t" (x));
  return x;
}

long double
sinl (long double x)
{
  int quadrant;
  long double r = reduce (x, &quadrant);

  switch (quadrant)
    {
    case 0: return sin_x87 (r);
    case 1: return cos_x87 (r);
    case 2: return -sin_x87 (r);
    default: return -cos_x87 (r);
    }
}

long double
cosl (long double x)
{
  int quadrant;
  long double r = reduce (x, &quadrant);

  switch (quadrant)
    {
    case 0: return cos_x87 (r);
    case 1: return -sin_x87 (r);
    case 2: return -cos_x87 (r);
    default: return sin_x87 (r);
    }
}

long double
tanl (long double x)
{
  int quadrant;
  long double r = reduce (x, &quadrant);

  /* fptan pushes 1.0 above the result, for compatibility with the 8087. */
  __asm__ ("fptan; fstp %%st(0)" : "+t" (r));
  return quadrant & 1 ? -1.0L / r : r;
}

long double
atanl (long double x)
{
  return atan2_x87 (x, 1.0L);
}

long double
asinl (long double x)
{
  return atan2_x87 (x, sqrtl ((1.0L - x) * (1.0L + x)));
}

long double
acosl (long double x)
{
  return atan2_x87 (sqrtl ((1.0L - x) * (1.0L + x)), x);
}

long double
expl (long double x)
{
  return exp_x87 (x);
}

long double
logl (long double x)
{
  /* ln x = ln 2 * log2 x, and fyl2x computes st(1) * log2 st(0). */
  __asm__ ("fldln2; fxch %%st(1); fyl2x" : "+t" (x));
  return x;
}

long double
powl (long double x, long double y)
{
  long double product, whole, half;
  int odd;

  if (y == 0.0L)
    return 1.0L;
  if (x == 0.0L)
    return y > 0.0L ? 0.0L : 1.0L / 0.0L;

  if (x > 0.0L)
    {
      /* fyl2x gives y * log2 x in one step, without rounding log2 x. */
      __asm__ ("fyl2x" : "=t" (product) : "0" (x), "u" (y) : "st(1)");
      return exp2_x87 (product);
    }

  /* A negative x has a real power only when y is an integer. */
  __asm__ ("frndint" : "=t" (whole) : "0" (y));
  if (whole != y)
    return (x - x) / (x - x);
  half = y * 0.5L;
  __asm__ ("frndint" : "=t" (whole) : "0" (half));
  odd = whole != half;
  return odd ? -powl (-x, y) : powl (-x, y);
}

long double
sinhl (long double x)
{
  long double a = abs_x87 (x), result;

  if (a < 1.0L)
    {
      long double u = expm1_x87 (a);
      result = (u + u / (u + 1.0L)) / 2.0L;
    }
  else
    {
      long double e = expl (a);
      result = (e - 1.0L / e) / 2.0L;
    }
  return x < 0.0L ? -result : result;
}

long double
coshl (long double x)
{
  long double e = expl (abs_x87 (x));
  return (e + 1.0L / e) / 2.0L;
}

long double
tanhl (long double x)
{
  long double a = abs_x87 (x), result;

  /* Beyond 23, tanh is 1 to the 64 bits of a long double. */
  if (a > 23.0L)
    result = 1.0L;
  else
    {
      long double u = expm1_x87 (2.0L * a);
      result = u / (u + 2.0L);
    }
  return x < 0.0L ? -result : result;
}

double sqrt (double x) { return sqrtl (x); }
double sin (double x) { return sinl (x); }
double cos (double x) { return cosl (x); }
double tan (double x) { return tanl (x); }
double atan (double x) { return atanl (x); }
double asin (double x) { return asinl (x); }
double acos (double x) { return acosl (x); }
double exp (double x) { return expl (x); }
double log (double x) { return logl (x); }
double pow (double x, double y) { return powl (x, y); }
double sinh (double x) { return sinhl (x); }
double cosh (double x) { return coshl (x); }
double tanh (double x) { return tanhl (x); }

float sqrtf (float x) { return sqrtl (x); }
float sinf (float x) { return sinl (x); }
float cosf (float x) { return cosl (x); }
float tanf (float x) { return tanl (x); }
float atanf (float x) { return atanl (x); }
float asinf (float x) { return asinl (x); }
float acosf (float x) { return acosl (x); }
float expf (float x) { return expl (x); }
float logf (float x) { return logl (x); }
float powf (float x, float y) { return powl (x, y); }
float sinhf (float x) { return sinhl (x); }
float coshf (float x) { return coshl (x); }
float tanhf (float x) { return tanhl (x); }
