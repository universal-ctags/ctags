#include <string>

// All of these are valid variable declarations

struct Struct1
{
	unsigned int m01;
	std::string m02;
	std::string ** m03, m04;
	std::string & (*m05)(int a,int b);
	std::basic_string<wchar_t, std::char_traits<wchar_t>, std::allocator<wchar_t> > m06[10];
} v01, v02[10];

enum Enum1
{
	E1
} v03, * v04;

struct Struct1 v05, * v06 = NULL;
std::string v07("test"), v08("test");
std::basic_string<wchar_t, std::char_traits<wchar_t>, std::allocator<wchar_t> > ** v09;

int main(int argc,char ** argv)
{
	unsigned short int l01;
	unsigned long long int * const l02 = NULL, * l03 = NULL;
	register int ** l04 = 0;

	std::string l05;
	std::string l06("test");
	std::string l07 = "test";
	const std::string & l08 = l07, l09 = l07;

	const void * (*l10)() = NULL;
	unsigned long int & (*l11)(void *);
	std::string & (*l12)(void);
	std::string ** (*l13)(int a,int b);

	std::basic_string<wchar_t, std::char_traits<wchar_t>, std::allocator<wchar_t> > l14;

	struct Struct1 l15;
	struct Struct1 * l16, l17, l18[10];
	Struct1 l19 = {};

	return 0;
}

// All of these are NOT valid variable declarations (but still valid C++)
typedef struct X n01;
typedef unsigned short int n02;
typedef enum Enum1 n03;

// This would be nice: handle #defines as ignore tokens.
//#define MY_API __declspec(dllexport)
//class MY_API n04;

#ifdef _MSVC
	class __declspec(dllexport) n05;
#endif
