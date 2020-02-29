const int V = 10;

template<typename T,int I = V << 1 > void funcA(T t)
{
}

typedef int K;

template<typename T,int I,int J = I < V> void funcB(T t)
{
}

template<typename T,int I,int J = I < V> K funcC(T t)
{
	return 0;
}

// This is allowed by C++03
template<typename T,int I,int J = I >> 2> K funcD(T t)
{
	return 0;
}

template<typename T,int I,int J = 1 >> I> K funcE(T t)
{
	return 0;
}

template<typename T,int I,int L,int J = I >> L> K funcF(T t)
{
	return 0;
}
