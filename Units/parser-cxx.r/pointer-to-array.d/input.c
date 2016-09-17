
#include <stdio.h>

typedef int (*int2ptr)[2];
typedef int (*(*int2ptr_func)(void))[2];

static int (*foo(void))[2] {
  static int foo_arr[2] = {1, 2};
  return &foo_arr;
}

int2ptr (*baz)(void) = foo;
int2ptr_func bar = foo;

int main(void) {
  int (*arr)[] = foo();
  printf("array[0] = %d\n", (*arr)[0]);
  printf("array[1] = %d\n", (*arr)[1]);
  return 0;
}
