class Base
{
public:
	virtual void foo() = 0;
};

class Derived final : public Base
{
	virtual void foo();
	virtual void final();
};

void Base::foo()
{
}

void Derived::foo()
{
}

void Derived::final()
{
}
