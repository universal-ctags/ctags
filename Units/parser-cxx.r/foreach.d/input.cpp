
// In real world this would have a (rather complex) implementation.
// See Q_FOREACH() and foreach() macros in Qt as example.
#define foreach(_a,_b)

char * pointers[10];
int a,b;

int main(int,char **)
{
	
	//...
	
	// p is declared inside foreach() parenthesis.
	// pointers is NOT declared here
	foreach(char * p,pointers)
	{

	}

	// This is not a variable declaration.
	if(a * b)
	{
	}

	return 0;
}
