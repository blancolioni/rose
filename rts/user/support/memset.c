/* Set n bytes at dest to c.

   It writes four bytes at a time, then the bytes that are left (#787). */

typedef unsigned int word __attribute__ ((may_alias));

void *
memset(void *dest, int c, unsigned int n)
{
  word fill = (unsigned char) c;
  word *dw = (word *) dest;
  char *d;

  fill |= fill << 8;
  fill |= fill << 16;

  while (n >= sizeof (word))
  {
    *dw++ = fill;
    n -= sizeof (word);
  }

  d = (char *) dw;
  while (n--)
  {
    *d++ = c;
  }
  return dest;
}
