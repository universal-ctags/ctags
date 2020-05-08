// template keyword used as disambiguator for dependent names

#include <type_traits>

struct Checks
{
	template <typename X> static constexpr bool Check()
	{
		return true;
	}
};

template<typename A, typename std::enable_if<Checks::template Check<A>(),bool>::type = true> class S
{
	template<typename U> void foo(){}
};

template<typename T> void bar()
{
	S<T> s;
	s.template foo<T>();
}

// Marker to make sure the parser doesn't bail out before this line.
int marker;
