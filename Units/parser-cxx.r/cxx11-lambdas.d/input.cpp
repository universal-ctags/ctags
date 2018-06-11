// This code compiles with
//  gcc -std=c++11 input.cpp

template<typename T> int call(T x)
{
}

template<typename A,typename B> class X
{
};

int test()
{
	auto l1 = [] { return 0; };

	auto l2 = [](int a,int b) { return a > b ? a : b; };

	auto l3 = [=](int a,int b) -> int {
		auto l4 = [a,b](int c){ return a+b+c; };
		return l4(5);
	};
	
	auto l5 = [](X<int,int> x,X<long,long>) -> X<double,double> { return X<double,double>(); };

	int v1 = call(
			[l1,l2,l3](int a,int b) -> int {
				return l1() + l2(a,b) + l3(a,b);
			}
	);
}

auto lg1 = [] { return 0; };

auto lg2 = [](int a,int b) { return a > b ? a : b; };

auto lg3 = [=](int a,int b) -> int {
	auto lg4 = [a,b](int c){ return a+b+c; };
	return lg4(5);
};

auto lg5 = [](X<int,int> x,X<long,long>) -> X<double,double> { return X<double,double>(); };
