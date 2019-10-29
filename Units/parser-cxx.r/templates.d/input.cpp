template <typename T> int 
zero (const T &v1, const T &v2)
{
  return 0;
}

template <typename T> inline T 
min(const T&v1, const T&v2)
{
  if (v1 < v2)
    return v1;
  else
    return v2;
}

template <class Type> 
class Item
{
  Item(const Type &t) : item (t), next (0) { }
  Type item;
  Item *next;
};

template<typename T> const T constant = T(10.0);