#include <string>
#include <vector>

struct Struct
{
};

enum Enum
{
};

class Class
{
};

union Union
{
};

void p00();
int p01();
unsigned int p02();
auto p03() -> int;
int * p04();
const unsigned long long int & p05();
static inline int * p06();
std::string & p07();
std::vector<std::string> * p08();
auto p09() -> std::vector<std::string> ***;
auto p10() -> std::string;
auto p11() -> int (*)(int);
struct Struct p12();
Struct p13();
union Union p14();
Union p15();
class Class * p15p();
Class & p16();
const enum Enum p17();
Enum p18();

void f00()
{
}

int f01()
{
	return 0;
}

unsigned int f02()
{
	return 0;
}

auto f03() -> int
{
	return 0;
}

int * f04()
{
	return 0;
}

const unsigned long long int & f05()
{
	unsigned long long int x;
	return x;
}

static inline int * f06()
{
	return 0;
}

std::string & f07()
{
	std::string x;
	return x;
}

std::vector<std::string> * f08()
{
	return 0;
}

auto p09() -> std::vector<std::string> ***
{
	return 0;
}

auto f10() -> std::string
{
	std::string x;
	return x;
}

auto f11() -> int (*)(int)
{
	return 0;
}

template<typename A> std::vector<A> * f13()
{
	return 0;
}

template<typename A> auto f14() -> std::vector<A> *
{
	return 0;
}

class X
{
public:
	void m00();
	constexpr int m01(){ return 0; };
	unsigned int m02();
	static auto m03() -> int;
	int * m04();
	const unsigned long long int & m05();
	static inline int * m06();
	virtual std::string & m07();
	std::vector<std::string> * m08();
	auto m09() -> std::vector<std::string> ***;
	virtual auto m10() const -> std::string;
	virtual auto m11() const -> int (*)(int);
};
