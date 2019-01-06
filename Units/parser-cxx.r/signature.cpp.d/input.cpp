/* Tests for collection of signature */

void foo (int a, char b) {}

/* note that K&R style declarations are not valid in C++ */
//int bar (a, b) int a; char b; {}

char *BAR::bar (char *c, double d[]) const {}

void foobar __ARGS ((int a, char b));


void params1(const char * c = "blah");
void params2(char x = 'x');
void params3(char x = ' ');
void params4(char x = ',');
void params5(char x = '\n');
void params6(char x = '\t',int n = 10,const char * v = "a string with\na newline");
void params7(char x = '	',	// This is  tab char
	     float p = 3.14,
	     const char * v = "a string with a tab char:	"
	     );
