void *
memset(void *dest, int c, unsigned int n)
{
  char *d=(char*)dest;
  while(n--)
  {
    *d++ = c;
  }
  return dest;
}
