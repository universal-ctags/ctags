// -*- c++ -*-
template<class A> class B {
	A a;
};
template<class A> class C {
  class B<A> b;
};
