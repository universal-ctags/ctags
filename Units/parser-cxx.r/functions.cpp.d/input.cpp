#include <string>

// Valid function prototypes
int p01(int p01p01,int p01p02);
unsigned short int * p02(unsigned int & p02p01,...);
auto p03(const int & p03p01,void * p03p02) -> const int &;
auto p04() -> int (*)(int);


// Valid function declarations
int f01(int f01p01,int f01p02)
{
	return 0;
}

unsigned short int * f02(unsigned int & f02p01,...)
{
	return 0;
}

auto f03(const int & f03p01,void * f03p02) -> const int &
{
	return 0;
}

auto f04() -> int (*)(int)
{
	return 0;
}


// Things that might look similar to function prototypes but are NOT function prototypes (but still valid C++)
std::string x01("test");


