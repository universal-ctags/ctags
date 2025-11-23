template <class T>
struct Point {
  int distance(void) {
    return 0;
  }
};

namespace NS {
  using MyReal = float;
  template <class T>
  using MyVec = T[2];
};

template<>
int Point<int>::distance(void) { return 1; }

template<>
int Point<NS::MyReal>::distance(void) { return 1; }

template<>
int Point<NS::MyVec<int>>::distance(void) { return 1; }
