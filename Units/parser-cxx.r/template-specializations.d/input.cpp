template<typename T>
struct A {
    void f(T); // member, declared in the primary template
    void h(T) {} // member, defined in the primary template
    template<class X1> void g1(T, X1); // member template
    template<class X2> void g2(T, X2); // member template
};

// specialization of a member
template<> void A<int>::f(int);
// member specialization OK even if defined in-class
template<> void A<int>::h(int) {}

// out of class member template definition
template<class T>
template<class X1> void A<T>::g1(T, X1) { }

// member template specialization
template<>
template<class X1> void A<int>::g1(int, X1);

// member template specialization
template<>
template<> void A<int>::g2<char>(int, char); // for X2 = char
// same, using template argument deduction (X1 = char)
template<>
template<> void A<int>::g1(int, char);

template<typename X> void m(X)
{
}

template<> void m<int>(int)
{
}

template<> void m<A<int>>(A<int> a)
{
}

// bug #2181
struct B { };
template<> void m<B>(B)
{
}

template<> void m(char)
{
}

#if HANDLE_BROKEN_INPUT
	// This is broken input. Should *not* be extracted.
	template <> void int<int>(int a);
#endif


// Examples from the "manual".

// primary template
template<class T1, class T2, int I>
class C {};

//partial specialization where T2 is a pointer to T1
template<class T, int I>
class C<T, T*, I> {};

// partial specialization where T1 is a pointer
template<class T, class T2, int I>
class C<T*, T2, I> {};

// partial specialization where T1 is int, I is 5, and T2 is a pointer
template<class T>
class C<int, T*, 5> {};

// partial specialization where T2 is a pointer
template<class X, class T, int I>
class C<X, T*, I> {};
