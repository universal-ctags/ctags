/* Variables */
char f [] __attribute__((section("__ksymtab_strings"), used, aligned(1)));

/* Taken form gcc's info document. */
struct duart a __attribute__ ((section ("DUART_A"))) = { 0 };
struct duart b __attribute__ ((section ("DUART_B"))) = { 0 };
char stack[10000] __attribute__ ((section ("STACK"))) = { 0 };
int init_data __attribute__ ((section ("INITDATA")));
