void *
memmove(void *dest, const void *src, unsigned int n)
{
  char *d=(char*)dest;
  char *s=(char*)src;

  /* When dest overlaps the end of src, copy from the end, so that each
     byte is read before it is overwritten (#441). */

  if (d > s && d < s + n)
  {
    d += n;
    s += n;
    while(n--)
    {
      *--d = *--s;
    }
  }
  else
  {
    while(n--)
    {
      *d++ = *s++;
    }
  }
  return dest;
}
