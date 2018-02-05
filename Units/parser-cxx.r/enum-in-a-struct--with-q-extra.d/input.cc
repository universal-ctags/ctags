namespace S {
  struct T {
    enum E {
      alpha, beta,
    } elt;
  };
}

struct S::T s = { .elt = S::T::E::alpha };
