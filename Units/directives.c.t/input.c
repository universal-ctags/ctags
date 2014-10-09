/* Test simple cases */
#define VARIABLE_LIKE	some_value
#define FUNCTION_LIKE(a,b)  (a + b)
#pragma weak WeakSymbol = StrongSymbol

#define with_long_comment  /* line 1
			      line 2 */

/* Test usual case */
#ifdef MY_MACRO
# define MACRO_TO_SEE1 1
int a;
#elif YOUR_MACRO
# define MACRO_TO_SEE2 2
int b;
#elif defined (THEIR_MACRO)
# define MACRO_TO_SEE3 3
int c;
#else
# define MACRO_TO_SEE4 4
int d;
#endif

/* Test commented-out case */
#if 0
#define IGNORE_MACRO
int e;
# if 0
#  define ANOTHER_IGNORE_MACRO
int f;
# else
#  define YAIM
# endif
#else
# define SEE_THIS_MACRO 1
int g;
#endif

/* Test path selection algorithm */
#ifdef OK
#define PATH1
int foo1 (void)
{
#elif defined (OK)
#define PATH2
int foo2 (void)
{
#else
#define PATH3
int foo3 (void)
{
#endif
}

int bar1 (void)
{
#ifdef OK
#define PATH1b
}
int p1;
#elif defined (OK)
#define PATH2b
}
int p2;
#else
#define PATH3b
}
int p3;
#endif
