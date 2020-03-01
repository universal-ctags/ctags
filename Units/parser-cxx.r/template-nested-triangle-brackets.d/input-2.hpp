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

// This stuff is allowed by C++03
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

template<typename T,int I,int J = 1 >> V> K funcG(T t)
{
	return 0;
}

template<typename T,int I,int J = I >> V> K funcH(T t)
{
	return 0;
}

template<typename T,int I,int J = 1 >> (1+2)> K funcI(T t)
{
	return 0;
}



template<typename T,int I,int J = I > 2> K funcJ(T t)
{
	return 0;
}

template<typename T,int I,int J = 1 > I> K funcK(T t)
{
	return 0;
}

template<typename T,int I,int L,int J = I > L> K funcL(T t)
{
	return 0;
}

/*

Without the knowledge of V these two are too ambiguous.

template<typename T,int I,long int J = 1 > V> K funcM(T t)
{
	return 0;
}

template<typename T,int I,unsigned long int J = I > V> K funcN(T t)
{
	return 0;
}
*/

template<typename T,int I,unsigned int J = 1 > (1+2)> K funcO(T t)
{
	return 0;
}









