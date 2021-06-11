#include <string>
#include <array>

// All the m*, l* and v* are valid variable declarations. The n* are NOT valid declarations.

struct Struct1
{
	unsigned int m01;
	std::string m02;
	std::string ** m03, m04;
	std::string & (*m05)(int a,int b);
	std::basic_string<wchar_t, std::char_traits<wchar_t>, std::allocator<wchar_t> > m06[10];
	std::string m07 { "test" }; // C++11
	std::string m08[2] { "a", "b" };
	int m09 {};
	int m10[2][2]{{1, 2}, {3, 4}};
	std::array<int, 3> m11 { {1,2,3} };
	std::string m12[3] { std::string("one"),"two",{'t', 'h', 'r', 'e', 'e'} };
	int m13 {false};
	std::string * m14 { new std::string("test") };
	decltype(int) m15;
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

#define SIZE 25
	std::string ** l20[SIZE];
	std::string l21[1 << 2];
	std::string * l22[SIZE][SIZE];
	std::string l23[5][2];
	std::string * const l24 = 0;

	wchar_t l25[] = { L"кошка" };
	wchar_t l26[] { L'к', L'о', L'ш', L'к', L'а', L'\0' };
	std::string l27[] = { std::string("one"),"two",{'t', 'h', 'r', 'e', 'e'} };
	int l28 {};
	bool l29 { false };
	std::string * l30{ new std::string("test") };
	std::string * l31(new std::string("test"));

	auto l32 = new std::basic_string<wchar_t, std::char_traits<wchar_t>, std::allocator<wchar_t> >;

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

// Note that function parameters are NOT extracted in this test.

template<typename X> X func(X p1)
{
	return p1+1;
}

int anotherFunc(int n06)
{
	func<int>(n06);

	void (Struct1::*l33)() = NULL;
	void (Struct1::*l34)(int) = NULL;
	int (Struct1::*l35)(int) = NULL;
	decltype(l36) l35;
}
