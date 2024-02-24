// See #3943.
// static struct S {int s;} fS1(int i);

/* This is invalid input as C++ code. */
static struct T {int t;} f1(int i)
{
  struct T r = {.t = i};
  return r;
}
