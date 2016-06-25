class Base
{
public:
	virtual void foo() noexcept = 0;
	virtual void bar() const noexcept = 0;
	int baz() noexcept { return 42; }
};
