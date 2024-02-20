export module Y;

export {
  namespace Z0 {
    int z0;
  };
}

namespace X {
  int a;
  export {
    int x;
    int f(int i);
  }
  int b;
};


int m;
export {
  int z;
  enum E { a = 1 };
}
int n;
