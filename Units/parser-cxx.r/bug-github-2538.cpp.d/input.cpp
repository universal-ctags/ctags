// Recursive macro expansion causes a segmentation fault
// or infinite loop

struct test
{
	int x;
};

int f(int v)
{
	struct test y;
	y.x = 10;
	return v & M;
}
