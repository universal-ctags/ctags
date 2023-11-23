struct x {
  int y;
};

enum X {
	A, B
};

extern int i;
int f(int j)
{
	if (j == 0)
		return 0;
	return i + B + j + f(0);
}
