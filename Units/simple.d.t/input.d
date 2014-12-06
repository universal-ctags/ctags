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

/+ 
int error;
 +/

static if (is(typeof(__traits(getMember, a, name)) == function))
	T conditional;

static assert( num < TL.length, "Name '"~name~"' is not found");

void main(string[] args)
{
	auto foo = new Foo(1337);

	writefln("%s", foo.bar());
}
