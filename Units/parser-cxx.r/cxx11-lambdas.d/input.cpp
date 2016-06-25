// This code compiles with
//  gcc -std=c++11 input.cpp

template<typename T> void call(T x)
{
}

int test()
{
	auto l1 = [] { return 0; };

	auto l2 = [](int a,int b) { return a > b ? a : b; };

	auto l3 = [=](int a,int b) -> int {
		auto l4 = [a,b](int c){ return a+b+c; };
		return l4(5);
	};
	
	call(
			[l1,l2,l3](int a,int b) -> int {
				return l1() + l2(a,b) + l3(a,b);
			}
	);
}
