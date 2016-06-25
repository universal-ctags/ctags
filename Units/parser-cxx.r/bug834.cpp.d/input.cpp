/*
  github issue 834 reported by hierabyss on 2016.03.12
  
  [...] after run ctags test1.cpp, there is no tag in the tags file.

*/

#include <vector>

using namespace std;

vector<vector<int>> C;

struct A {
    int a;
    int b;
};
