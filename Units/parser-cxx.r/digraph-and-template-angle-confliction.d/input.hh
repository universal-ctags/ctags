#include <netinet/ip.h>
#include <map>

namespace std {
  template <>
  struct hash<::sockaddr_in> {
    int a;
  };
};

int A <::> = {1, 2};
int B <:2:> = {1, 2};
#define N 2
int C <:N:> = {1, 2};
int D [] = {1, 2};

const int M = 3;
int E <:::M:> = {1, 2};

int foo (void)
{
  return 0;
}
