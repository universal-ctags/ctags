// typeof_unqual (C23) must be handled like typeof.
const int a = 1;
typeof(a) var1 = 2;
typeof_unqual(a) var2 = 3;
typeof_unqual(const int) var3 = 4;
static typeof_unqual(a) const var4 = 5;

struct S {
	typeof_unqual(a) member;
};

void f(typeof_unqual(a) param) {}
typeof_unqual(a) g(void) { return 0; }

// GNU spellings
__typeof_unqual(a) var5 = 6;
__typeof_unqual__(const int) var6 = 7;
