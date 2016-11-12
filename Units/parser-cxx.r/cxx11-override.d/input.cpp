class Base
{
public:
	virtual void foo() = 0;
};

class Derived : public Base
{
	virtual void foo() override;
	virtual void foo() const override;
	virtual void override();
};

void Base::foo()
{
}

void Derived::foo()
{
}

void Derived::override()
{
}
