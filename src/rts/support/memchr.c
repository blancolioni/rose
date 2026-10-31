void *
memchr(const void *s, int c, unsigned int n)
{
    const unsigned char *p = s;

    for ( ; n-- ; p++) {
	if (*p == (unsigned char) c) {
	    return (void *) p;
	}
    }
    return 0;
}
