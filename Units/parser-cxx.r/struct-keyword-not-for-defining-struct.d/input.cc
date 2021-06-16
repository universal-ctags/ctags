// Taken from #2840 submitted by @chongchal
typedef struct Currency
{
    int Dollar;
    int Cents;

    struct Currency &operator=(Currency& value)
    {
        Dollar = value.Dollar;
        Cents = value.Cents;
        return *this;
    }
};

void test()
{
}
