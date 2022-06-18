namespace X {
template<typename T> class C;
};

template<typename T> class X::C
{
public:
  C<T>(void) {
  }
};

template<typename T> class D
{
public:
  D<T>(void) {
  }
};
