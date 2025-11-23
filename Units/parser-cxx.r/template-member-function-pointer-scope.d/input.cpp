// Derived from #4348 submitted by @gaborbernat.

// C++ template specialization that previously triggered scope management crash
// Test case for issue 4344: assertion failure in cxxScopePushTop
template <class T>
struct TestUtil {
    static bool isNull(const T&) { return false; }
};

template <class T>
struct TestUtil<T*> {
    static bool isNull(T* f) { return 0 == f; }
};

struct memtype {};
struct klass {};

template <>
inline
bool
TestUtil<const memtype klass::*&>::isNull(const memtype klass::* &f)
{
    return 0 == f;
}

// Additional test cases for complex template specializations
template <typename T>
struct Handler {
    void process(T value) { }
};

template <typename R, typename C>
struct Handler<R (C::*)()> {
    void process(R (C::*method)()) { }
};

template <typename R, typename C, typename A>
struct Handler<R (C::*)(A)> {
    void process(R (C::*method)(A)) { }
};
