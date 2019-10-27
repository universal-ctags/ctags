template <class T> class C0 {
  T t;
};

template <class T, typename T0 = C0<const T>> class C1 {
  T0 t0;
public:
  void c1rehash() {}
};

template <class T, typename T1 = C0<C0<const T>>> class C2 {
  T1 t1;
public:
  void c2rehash() {}
};

template <class T, typename T2 = C0<C0<C0<const T>>>> class C3 {
  T2 t2;
public:
  void c3rehash() {}
};

template <class T, typename T3 = C0<C0<C0<C0<const T>>>>> class C4 {
  T3 t3;
public:
  void c4rehash() {}
};

template <class T, typename T4 = C0<C0<C0<C0<C0<const T>>>>>> class C5 {
  T4 t4;
public:
  void c5rehash() {}
};

template <class T, typename T3 = C0<C0<C0<C0<const T>>>>, class S = int> class C4x1 {
  T3 t3;
  S  s;
public:
  void c4x1rehash() {}
};

template <class T, typename T3 = C0<C0<C0<C0<const T>>>>, class S = C0<const T>> class C4x2 {
  T3 t3;
  S  s;
public:
  void c4x2rehash() {}
};


int
main(void)
{
  C0 <int> c0;
  C1 <int> c1();
  C2 <int> c2();
  C3 <int> c3();
  C4 <int> c4();
  C4x1 <int, int, int> cx41();
  C4x2 <int, int, int> cx42();
  return 0;
}
