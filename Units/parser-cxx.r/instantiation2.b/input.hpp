// Based on an example in https://cpplover.blogspot.com/2013/12/c03c11_3905.html (in Japanese)
// A C++03 compiler accept this input but a C++11 compiler does not.
template <class T> struct X { };
template <int N> struct Y { };
#define M 2
struct S {
	int i;
	X< Y< 1 >> 2 > > x2;
	X< Y< M >> 2 > > x3;
	X< Y< M >> M > > x4;
	X< Y< 1 < 2 > > x5;
	X< Y< M < 2 > > x6;
	X< Y< M < M > > x7;
  /*    Too ambiguous even in C++03
	---------------------------
	X< Y< 1 > 2 > > x8;
	X< Y< M > 2 > > x9;
	X< Y< M > M > > xa;	*/
	double d;
};
