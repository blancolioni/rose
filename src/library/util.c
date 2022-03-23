/* things normally by the C standard library, which we don't use
 * directly, but GNAT does */

int
memcmp(void *a, void *b, unsigned int n)
{
    unsigned char u1, u2;

    for ( ; n-- ; a++, a++) {
	u1 = * (unsigned char *) a;
	u2 = * (unsigned char *) b;
	if ( u1 != u2) {
	    return u1 - u2;
	}
    }
    return 0;
}

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

void *
memmove(void *dest, const void *src, unsigned int n)
{
  char *d=(char*)dest;
  char *s=(char*)src;
  while(n--)
  {
    *d++ = *s++;
  }
  return dest;
}

void
gnat_last_chance_handler(void)
{
}

int heap_base;

