// Taken from https://cpprefjp.github.io/lang/cpp14/variable_templates.html
template <class T>
constexpr bool is_integral_v = false;

template <>
constexpr bool is_integral_v<int> = true;

int main()
{
  if (is_integral_v<int>) {}
}
