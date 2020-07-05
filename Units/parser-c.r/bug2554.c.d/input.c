/* valgrind reported an error when ctags built with
 * --enable-debugging configure option.
 *
 * To reproduce:
 *
 *  $ make units LANGUAGES=C VG=1
 *
 */
typedef struct
{
  int*n;
} S;

struct foo {
	int bar;
} v = {
	.bar = 1,
};

struct foo w = {
	.bar = 2,
};
