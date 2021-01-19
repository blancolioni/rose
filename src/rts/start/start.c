extern void adainit(void);
extern void main(void);
extern void exit(int);

void _start(void)
{
    adainit();
    main();
    exit(0);
}
