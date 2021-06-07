#include <string>
#include <memory>

class Type {};

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
	X operator && (const X &a);

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
	
	int operator[](const X &a) const
	{
		return 0;
	}
	
	// This should appear as member of the global namespace
	inline friend X operator*(const X &a, const X &b)
	{
		return X();
	}
	
	// This should NOT appear at all
	friend X operator && (const X &a,const X & b);
	
	void * operator new(size_t);
	void operator delete(void *);
	void * operator new[](size_t);
	void operator delete[](void *);

	operator Type() const;

	// requires -std=c++20 to compile
	auto operator<=>(const X&a) const -> decltype(1 <=> 2)
	{
		return std::strong_ordering::less;
	}
};

X & X::operator *= (int x)
{
	return *this;
}

X & X::operator *= (const X & x)
{
	return *this;
}

X X::operator && (const X &a)
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

X::operator Type() const
{
	return Type();
}

#ifdef DONT_CARE_ABOUT_COMPILATION
// This doesn't compile because it lacks the necessary definitions. But it's still extracted.

template<typename T> inline cv::Affine3<T>::operator Eigen::Transform<T, 3, Eigen::Affine, (Eigen::RowMajor)>() const
{
	Eigen::Transform<T, 3, Eigen::Affine, (Eigen::RowMajor)> r;
	cv::Mat hdr(4, 4, cv::traits::Type<T>::value, r.matrix().data());
	cv::Mat(matrix, false).copyTo(hdr);
	return r;
}

#endif
