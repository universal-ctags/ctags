// Reported by Przemyslaw Szymanski on 02.06.2016

template <typename T = void, bool B1 = true> // (1) fail if there are multiple params with default values
class A : public T
{
public:
    typedef A<T, B1> this_type;

    A(int i ) : m_i(i)
    {
    }

    static A create(int i)
    {
        return this_type(i); // (2) fail here
    }

    static int g_i;
private:
    int m_i;
};

template <typename T, bool B1>
int A<T, B1>::g_i; // (3) fail during parsing static member definition