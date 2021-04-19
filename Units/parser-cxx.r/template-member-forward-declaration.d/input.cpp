template <class Class>
class A
{
public:
	A() { p = new Class(); }
	Class *p;
};

class B {
public:
	// forward declaration okay because only used as pointer in Template
	A<class C> a_c_forward;
	A<struct S> a_s_forward;
	A<union U> a_u_forward;
};

