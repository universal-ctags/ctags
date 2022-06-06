// Based on an example in https://cpplover.blogspot.com/2013/12/c03c11_3905.html (in Japanese)

template <class T> struct X { };
template <int N> struct Y { };
#define M 2
struct S {
	int i;
	Y<0> y;
	X< Y< 1 > > x0;
	X< Y< 1 + 2 > > x1;
	X< Y< 1 << 2 > > x2;
	X< Y< M << 2 > > x3;
	X< Y< M << M > > x4;
	double d;
};
