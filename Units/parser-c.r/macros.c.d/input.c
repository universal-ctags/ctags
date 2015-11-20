#define VARIABLE_LIKE	some_value
#define FUNCTION_LIKE(a,b)  (a + b)
#pragma weak WeakSymbol = StrongSymbol

/* handling of spoofing macros */
MACRO1(foo);
MACRO2(bar);
MACRO3(x, y);

void prototype1 __ARGS((int arg1, void *arg2));
void prototype2 __ARGS((int arg1, void *arg2))
{
}
#undef FUNCTION_LIKE
#undef VARIABLE_LIKE
