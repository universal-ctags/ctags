#define VARIABLE_LIKE	some_value
#define FUNCTION_LIKE(a,b)  (a + b)
#pragma weak WeakSymbol = StrongSymbol

/* handling of spoofing macros */
DECL1(foo); /* gcc will accept this as function prototype (with some warnings) */
MACRO2(bar);
DECL3(x, y); /* gcc will accept this as function prototype (with some warnings) */

void prototype1 __ARGS((int arg1, void *arg2));
void prototype2 __ARGS((int arg1, void *arg2))
{
}
#undef FUNCTION_LIKE
#undef VARIABLE_LIKE
