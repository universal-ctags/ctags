#include <string>
#include <memory>

namespace n01
{
	class c01
	{
	};

	namespace n02
	{
		typedef unsigned int type01;

		// These declarations are required by C++ standard for the ones below to compile properly
		auto f06(n01::c01 && f06a01) -> type01 *;
		auto p06(n01::c01 && p06a01) -> type01 *;
	}
};

// Valid function prototypes
int p01(int p01a01,int p01a02);
unsigned short int * p02(unsigned int & p02a01,...);
auto p03(const int & p03a01,void * p03a02) -> const int &;
auto p04() -> int (*)(int);
static std::string p05(const int *** p05a01);
auto n01::n02::p06(n01::c01 && p06a01) -> n01::n02::type01 *;
unsigned int p07(int (*p07a01)(int * x1,int x2),...);
void (*p08(void (*)(int *p08a01)))(int *);

// Valid function declarations
int f01(int f01a01,int f01a02)
{
	return 0;
}

unsigned short int * f02(unsigned int & f02a01,...)
{
	return 0;
}

auto f03(const int & f03a01,void * f03a02) -> const int &
{
	return 0;
}

auto f04() -> int (*)(int)
{
	return 0;
}

static inline std::string f05(const int *** f05a01)
{
	return std::string();
}

auto n01::n02::f06(n01::c01 && f06a01) -> n01::n02::type01 *
{
	return 0;
}

unsigned int f07(int (*f07a01)(int * x1,int x2),...)
{
	return 0;
}

void (*f08(void (*)(int *f08a01)))(int *)
{
	return 0;
}

int f09(char *((*f09a01)()))
{
	return 0;
}

int f10(int f10a01,int f10a02[],int f10a03[2][3],int (f10a04)[],int (f10a05)[][5])
{
	return 0;
}

// Valid function templates
template <typename T> std::unique_ptr<T> t01(T && t01a01)
{
    return std::unique_ptr<T>(NULL);
}

#define MACRO_TEXT ""

template <typename T> auto t02(T && t02a01) -> std::unique_ptr<T>
{
	// throw may look like a prototype, but it isn't
	throw std::string(MACRO_TEXT);
    return std::unique_ptr<T>(NULL);
}

// Things that might look similar to function prototypes but are NOT function prototypes (but still valid C++)
std::string x01("test");

#if NOTVALIDCPP
	// This is not really valid C++ because it appears in the wrong context.
	// However we simulate the parser being wrong about the current state (it happens).
	// This should be NOT marked as a prototype even if found out of a function.
	throw std::string(MACRO_TEXT);
#endif
