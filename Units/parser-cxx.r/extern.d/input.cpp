// This was initially reported as bug 931@github reported by Corax26 on
// 12/05/2016: the extern "C" declarations were missing from the ctags output.
//
// Additionally this test checks the +{c.properties} field for the "extern"
// property being properly set.

extern "C" void f(void);
extern "C" void g(void) {}

extern "C"
{
  // Please note that h() and i have internal linkage (but their name
  // mangling is C-style). h() is just a prototype while i is both
  // a declaration AND a definition of variable.
  void h(void);
  int i;

  // j has external linkage (declaration only).
  extern int j;
}
