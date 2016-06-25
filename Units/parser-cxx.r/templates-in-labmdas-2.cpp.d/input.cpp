// Bug reported by akrzyz on github.

namespace Foo
{
template <class T>
T bar() 
{
    return T{};
};
}

int main()
{
    auto l = []{return Foo::template bar<int>();};
    return l();
}
