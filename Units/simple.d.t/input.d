import std.stdio;

alias AliasInt = int;

struct Struct
{
	union Union
	{
		bool quxx;
		int qar;
	}
}

enum Enum : int
{
	foo,
	bar,
}

interface Interface
{
	public AliasInt bar();
}

class Class : Interface
{
	private AliasInt _bar;

	public this(AliasInt x)
	{
		this._bar = x;
	}

	public AliasInt bar()
	{
		return this._bar;
	}
}

__gshared int globalVar;

void main(string[] args)
{
	auto foo = new Class(1337);

	alias string AliasString;
	AliasString baz = "Hello, World!";

	writefln("%s", foo.bar());
}
