/* bug reported by akrzyz on 2016.03.10: no tags were emitted */

#include <vector>
#include <map>

template<template<class...> class Container, class Elem>
auto foo1(const Container<Elem> & p_container)
{
    return Container<Elem>{};
}

template<template<class...> class Container, class Key, class Elem>
auto foo2(const Container<Key,Elem> & p_container)
{
    return Container<Key,Elem>{};
}

void bar()
{
}

int main()
{
    auto v = foo1(std::vector<int>{1,2,3});
    auto m = foo2(std::map<int,int>{{1,2}});
    return 0;
}
