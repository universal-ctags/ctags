// Bug reported by akrzyz on github.

template<class T>
class Bar
{
public:
    template <class A>
    A foo()
    {
        return A{};
    }
};

int f1()
{
    Bar<int> b;
    auto t = b.template foo<int>();
    return t;
}

int f2()
{
    Bar<int> b;
    auto l = [](auto & p){ return p.template foo<int>();};
    return l(b);
}

int main()
{
    return f1() + f2();
}
