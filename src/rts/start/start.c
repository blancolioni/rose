extern void adainit(void);
extern void main(void);

void _start(void)
{
    adainit();
    main();
}
