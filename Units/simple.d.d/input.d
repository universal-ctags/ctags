module test.simple;

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
	
	protected:
	auto tfun(T)(T v)
	{
		return v;
	}
	
	class CT(T)
	{
		T member;
		enum missing = T.init; // FIXME
	}
}

public
{
	int modulevar;
}

// declaration templates
interface IT(T){}
struct ST(T){}
union UT(T){}
// FIXME - parsed as 'T'
//alias AT(T) = T;
//enum ET(T) = T.init;

template Template(alias a, T...)
{
	alias TemplateAlias = a!T;
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

__gshared int globalVar;

void main(string[] args)
{
	auto foo = new Class(1337);

	alias string AliasString;
	AliasString baz = "Hello, World!";

	writefln("%s", foo.bar());
}
