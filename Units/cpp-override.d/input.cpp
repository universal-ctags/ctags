class Base
{
public:
	virtual void foo() = 0;
};

class Derived : public Base
{
	virtual void foo() override;
};

void Base::foo()
{
}

void Derived::foo()
{
}
