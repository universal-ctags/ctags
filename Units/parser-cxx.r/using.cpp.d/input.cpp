#include <string>

using namespace std;
using std::string;
using x = std::string;

class A
{
public:
	void test();
	void operator ()() {};
	int operator +(int i) { return i; };
};

class B : public A
{
public:
	void test(x t);
	using A::test;
	using A::operator();
	using A::operator+;
};
