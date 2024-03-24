// Taken from https://cpprefjp.github.io/lang/cpp23/generalized_wording_for_partial_specializations.html
#include <iostream>

template <class T>
constexpr T zero = 0;

template <class T>
constexpr T* zero<T*> = nullptr;

int main() {
  int x = zero<int>;
  int* y = zero<int*>;

  std::cout << x << std::endl;
  std::cout << y << std::endl;
}
