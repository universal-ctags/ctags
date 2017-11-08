// The next one will be reconigzed as a macro.
// struct s (a);
int (A);
MACRO(ARGS);
int *(B);
struct s *(C);

int (a[1]);
int *(b[2]);
int (c[1][2]);
int *(d[2][3]);
int *(* const volatile * e[3]);

struct s {
	int *(* const volatile * f[3][3]);
	int (g[3]);
} t;

int (* volatile * (*h[1]));
