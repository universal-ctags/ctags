/*
Reported by masatake in issue #930@github.

.h files are always parsed in C++ mode, for safety.
However they may actually contain C code, which may
use C++ keywords as variable names.

This test checks the C++ parser capabilities to guess such occurences.


*/

extern int private;
static inline int public(void)
{
	return private;
}

typedef int protected;

