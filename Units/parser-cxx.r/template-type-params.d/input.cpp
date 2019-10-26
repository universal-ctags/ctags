template <typename T, typename S>
S f (T a, T b)
{
  return (S) (a + b);
}

template <>
int f (int a, int b)
{
  return a + b;
}

template <typename T>
class klass
{
public:
  T v;
  T g (T a)
  {
    return f<T>(a, v);
  }
};

template <class T>
using c = klass<T>;
