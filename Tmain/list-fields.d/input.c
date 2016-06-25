#include <stdio.h>
struct X {
  int i;
};

struct Y {
  int j;
  struct X x;
};

int main(void)
{
  return 0;
}

