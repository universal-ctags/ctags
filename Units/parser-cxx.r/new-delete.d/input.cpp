// Only fn() should be reported here.
Class fn()
{
	delete x;
	delete[] x;
	new Class();
	(void)new Class();
	return new Class();
}