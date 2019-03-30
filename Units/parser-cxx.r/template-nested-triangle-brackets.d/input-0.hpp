template <typename X>
class Foo {};

template <typename Y = Foo<int>>
constexpr Foo<Y> bar {};

const int i = 3;
