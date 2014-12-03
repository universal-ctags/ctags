import std.stdio;

class Foo
{
	private int _bar;

	public this(int x)
	{
		this._bar = x;
	}

	public int bar()
	{
		return this._bar;
	}
}

Object obj;

private:
int i;

void main(string[] args)
{
	auto foo = new Foo(1337);

	writefln("%s", foo.bar());
}
