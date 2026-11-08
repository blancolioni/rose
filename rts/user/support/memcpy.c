/* Copy n bytes from src to dest, which don't overlap.

   It copies four bytes at a time, then the bytes that are left. The x86
   reads and writes words at any address, so neither needs to be aligned
   (#787). */

typedef unsigned int word __attribute__ ((may_alias));

void *
memcpy(void *dest, const void *src, unsigned int n)
{
  word *dw = (word *) dest;
  const word *sw = (const word *) src;
  char *d;
  const char *s;

  while (n >= sizeof (word))
  {
    *dw++ = *sw++;
    n -= sizeof (word);
  }

  d = (char *) dw;
  s = (const char *) sw;
  while (n--)
  {
    *d++ = *s++;
  }
  return dest;
}
