#include <type_traits>

class A
{
public:
	template<typename X> 
		typename std::enable_if<2 << 3,int>::type f1(X x)
		{
			return (int)x;
		};
	
	template<typename X> 
		typename std::enable_if<!false,int>::type f2(X x)
		{
			return (int)x;
		};
};

class B
{
};