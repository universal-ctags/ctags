static void foo (int nelem)
{
    int rsize = 2;
    while (nelem < 0) {
	rsize <<= 1;
    }
}

static void bar (int value)
{
    return value < 0 ? value : 3;
}

static bar2 (void)
{
}
