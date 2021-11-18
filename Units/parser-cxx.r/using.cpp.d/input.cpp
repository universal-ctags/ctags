#include <string>

using namespace std;
using std::string;
using x = std::string;

class A
{
public:
	void test();
};

class B : public A
{
public:
	void test(x t);
	using A::test;
};

namespace X
{
	class C : A
	{
	public:
		void test2();
	};
}

class D : X::C
{
public:
	using X::C::test2;
};
