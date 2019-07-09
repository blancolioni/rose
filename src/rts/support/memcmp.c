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
