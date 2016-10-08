#include <string>

#define DECLARE_FUNCTION(_ret,_name) _ret _name();

DECLARE_FUNCTION(int,p1);
DECLARE_FUNCTION(std::string,p2);

#define DECLARE_FUNCTION_2(_ret,_name,...) _ret _name(__VA_ARGS__);

DECLARE_FUNCTION(int,p3,int a,int b);

#define DEPRECATED(...) __VA_ARGS__ __attribute__((deprecated))

DEPRECATED(int p4());

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