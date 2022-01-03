/* ctags -o output.tags --fields=+St -n input.c */

#define N 1
#define M 3

static int foo (int v)
{
	return v + N;
}

static void bar(char **argv, int *r)
{
	*r = M;
}

int main(int argc, char **argv)
{
	int i;

	bar(argv, &i);
	return foo (argc) + i;
}
