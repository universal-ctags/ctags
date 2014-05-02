/* C++11 introduced the "using name = type;" construct as an alternative
  to typedefs. Process them as typedefs. Contributed by Maxime Coste */

using Integer = int;

template <typename T>
using Vector = std::vector<T>;
