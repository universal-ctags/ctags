/* Taken from https://gcc.gnu.org/onlinedocs/gcc-3.3.1/gcc/Thread-Local.html#Thread-Local */
__thread int i;
extern __thread struct state s;
static __thread char *p;
