// _BitInt(N) (C23) is a type specifier, not a call.
_BitInt(128) g_var = 0;
unsigned _BitInt(8) g_uvar;
static _BitInt(64) const g_const = 1;

struct S {
	_BitInt(32) member;
};

void process_data(_BitInt(16) x) {}
void process_pair(int y, unsigned _BitInt(16) x);
unsigned _BitInt(12) twelve(unsigned _BitInt(12) v, int w) { return v; }
