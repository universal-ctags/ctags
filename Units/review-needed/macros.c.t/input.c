#define VARIABLE_LIKE	some_value
#define FUNCTION_LIKE(a,b)  (a + b)
#pragma weak WeakSymbol = StrongSymbol

/* handling of spoofing macros */
MACRO(foo);
void prototype __ARGS((int arg1, void *arg2));
