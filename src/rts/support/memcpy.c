void *
memcpy(void *dest, const void *src, unsigned int n)
{
  char *d=(char*)dest;
  char *s=(char*)src;
  while(n--)
  {
    *d++ = *s++;
  }
  return dest;
}
