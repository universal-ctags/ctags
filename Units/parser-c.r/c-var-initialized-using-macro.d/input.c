#define INIT(x) x

static const int i = INIT(2);
int j;
int k = 1;

int f(void)
{
	int l = INIT(3);
	return l + k + j + i;
}
