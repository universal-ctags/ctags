// Bug #931@github reported by Corax26 on 12/05/2016
// The extern "C" declarations were missing from the ctags output.
// Additionally this test checks the +{c.properties} field
// for the "extern" property being set. 

extern "C" void f(void);
extern "C" void g(void) {}

extern "C"
{
  void h(void);
  int i;
}