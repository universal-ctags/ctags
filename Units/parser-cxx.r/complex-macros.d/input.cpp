#include <string>

// Macro without parameters
#define DECLARE_FUNCTION_1 int p0()

DECLARE_FUNCTION_1;

// Macro without parameters
#define DECLARE_FUNCTION_1A() int p0A()

DECLARE_FUNCTION_1A;

// Simple macro with parameters

#define DECLARE_FUNCTION_2(_ret,_name) _ret _name();

DECLARE_FUNCTION_2(int,p1);
DECLARE_FUNCTION_2(std::string,p2);

// Var args

#define DECLARE_FUNCTION_3(_ret,_name,...) _ret _name(__VA_ARGS__);

DECLARE_FUNCTION_3(int,p3,int a,int b);

#define DEPRECATED(...) __VA_ARGS__ __attribute__((deprecated))

DEPRECATED(int p4());

// Recursive macro expansion

#define DECLARE_TWO_VERSIONS_OF_FUNCTIONS(_prefix1,_prefix2) \
	DECLARE_FUNCTION_2(int,_prefix1 ## a) \
	DECLARE_FUNCTION_2(int,_prefix2 ## a)

DECLARE_TWO_VERSIONS_OF_FUNCTIONS(p5,p6)

// Stringification
#define STRINGIFY(token) #token

const char * test = "" STRINGIFY(; int notVisible;);

// Unbalanced brackets (this comes from a nasty example in a github issue)
#define DECLARE_FUNCTION_4(Ret,Class,Method,...) Ret Class##__##Method(Class *that, ##__VA_ARGS__)
#define DECLARE_FUNCTION_4_BEGIN(...) { /*not really this, but this is what ctags should see*/
#define DECLARE_FUNCTION_4_END(...) } /*not really this, but this is what ctags should see*/

// Unbalanced brackets
DECLARE_FUNCTION_4(int,foo,bar,int x)
DECLARE_FUNCTION_4_BEGIN({)
DECLARE_FUNCTION_4_END(},{ /*dtors*/ })

// Token pasting

#define IMPLEMENT_FUNCTIONS(_prefix) \
	void _prefix ## a(){ }; \
	void _prefix ## b(){ };

IMPLEMENT_FUNCTIONS(f1);

#define DECLARE_VARS(_prefix) int _prefix ## a; int _prefix ## b;

int main(int,char **)
{
	DECLARE_VARS(l);
	return 0;
}