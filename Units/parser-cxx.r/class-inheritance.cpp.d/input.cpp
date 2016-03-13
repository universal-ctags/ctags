

class A
{
};

class B
{
};

template<typename X,typename Y> class C
{
};

class D : public A
{
};

class E : public A, public B
{
};

class F : private A, public B
{
};

class G : virtual D
{
};

class H : public A, virtual B
{
};

class I : public C<A,B>
{
};

class J : private A, public C<A,B>
{
};

