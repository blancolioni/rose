/* Copy n bytes from src to dest, which may overlap.

   Like memcpy, it copies four bytes at a time, then the bytes that are
   left (#787). */

typedef unsigned int word __attribute__ ((may_alias));

void *
memmove(void *dest, const void *src, unsigned int n)
{
  char *d = (char *) dest;
  const char *s = (const char *) src;

  /* When dest overlaps the end of src, copy from the end, so that each
     byte is read before it is overwritten (#441). */

  if (d > s && d < s + n)
  {
    d += n;
    s += n;
    while (n >= sizeof (word))
    {
      d -= sizeof (word);
      s -= sizeof (word);
      *(word *) d = *(const word *) s;
      n -= sizeof (word);
    }
    while (n--)
    {
      *--d = *--s;
    }
  }
  else
  {
    while (n >= sizeof (word))
    {
      *(word *) d = *(const word *) s;
      d += sizeof (word);
      s += sizeof (word);
      n -= sizeof (word);
    }
    while (n--)
    {
      *d++ = *s++;
    }
  }
  return dest;
}
