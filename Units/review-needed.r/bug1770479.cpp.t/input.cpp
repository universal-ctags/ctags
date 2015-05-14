#include <sstream>

int main (int argc, char **argv)
{
  std::ostringstream a;
  a << "a";

  std::ostringstream b;
  b << "b";

  return 0;
}

int foo (int i)
{
  return i;
}
