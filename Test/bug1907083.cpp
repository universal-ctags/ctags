// All of these should have "class:C", but m2-m4 have "class C::C" with ctags 5.7.
C::T * C::m1() {}
C::T * const C::m2() {}
C::T const * C::m3() {}
C::T const * const C::m4() {}
