extern void f0 (void) __attribute__ ((section ("bar0p")));
void f0 (void) __attribute__ ((section ("bar0")))
{
}

extern void __attribute__ ((section ("bar1p")))  f1 (void);
void __attribute__ ((section ("bar1"))) f1 (void)
{
}

extern __attribute__ ((section ("bar2p"))) void f2 (void);
__attribute__ ((section ("bar2"))) void f2 (void)
{
}

__attribute__ ((section ("bar3p"))) extern void f3 (void);
__attribute__ ((section ("bar3"))) extern void f3 (void)
{
}

__attribute__ ((section ("bar4p"))) static void f4 (void);
__attribute__ ((section ("bar4"))) static void f4 (void)
{
}
