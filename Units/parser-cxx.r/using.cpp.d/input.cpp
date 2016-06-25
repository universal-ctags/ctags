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