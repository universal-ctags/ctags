/* Taken from #4187 submitted by @emilioj, Emilio J. Padrón González */
float foo1(int a)
{
  for (int useless1=0; i<1000; ++i) {
    nonsense()
  }
}

bool foo2(float* pointer = NULL)
{
  for (int useless2=0; i<1000; ++i) {
    nonsense()
  }
}

bool foo2_alt(float* pointer)
{
  for (int useless2_alt=0; i<1000; ++i) {
    nonsense()
  }
}

bool foo3(const int a, float b, double c)
{
  for (int useless3=0; i<1000; ++i) {
    nonsense()
  }
}
