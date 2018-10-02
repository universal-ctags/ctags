static
void _MyFunction(void)
{

}

static
void MyOtherFunction(void)
{

}

class Foo
{
	int _Mumble(void);
	int Grumble(void);
};

int	Foo::_Mumble(void)
{
	_MyFunction();
	return 0;
}

int Foo::Grumble(void)
{
	MyOtherFunction();
	return 0;
}
