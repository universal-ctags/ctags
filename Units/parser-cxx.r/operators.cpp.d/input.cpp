#include <string>
#include <memory>

class X
{
public:
	X & operator = (const X &x)
	{
		return *this;
	}
	
	bool operator == (const X &x)
	{
		return true;
	}
	
	inline X & operator-=(const X &x)
	{
		return *this;
	}
	
	inline X & operator += (const X &x)
	{
		return *this;
	}
	
	X & operator *= (int x);
	X & operator *= (const X & x);
	
	inline void operator /= (int)
	{
	}
	
	inline void *** operator()()
	{
		return 0;
	}
	
	inline X & operator++()
	{
		return *this;
	}

	X & operator--()
	{
		return *this;
	}
	
	int operator[](int)
	{
		return 0;
	}
	
	// This should appear as member of the global namespace
	inline friend X operator*(const X &a, const X &b)
	{
		return *this;
	}
	
	// This should NOT appear at all
	friend X operator && (const X &a,const X & b);
	
	void * operator new(size_t);
	void operator delete(void *);
	void * operator new[](size_t);
	void operator delete[](void *);
};

X & X::operator *= (int x)
{
	return *this;
}

X & X::operator *= (const X & x)
{
	return *this;
}

X X::operator && (const X &a,const X & b)
{
	return *this;
}

void * X::operator new(size_t)
{
	return NULL;
}

void X::operator delete(void *)
{
}

void * X::operator new[](size_t)
{
	return NULL;
}

void X::operator delete[](void *)
{
}

int main(int argc,char ** argv)
{
	X x;
	return x[0];
}

