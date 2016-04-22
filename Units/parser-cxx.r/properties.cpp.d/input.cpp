
class C01
{
public:
	C01() = default;

	static int mp01;
	mutable int mp02;

	virtual void mf01();
	virtual void mf02() final;
};

class C02 : public C01
{
public:
	C02() = delete;
	explicit C02(int i)
	{
	}
	
	void mf01() override;
	virtual void mf03() = 0;
	virtual void mf04() final;
	static inline void mf05()
	{
	}
	inline void mf06() const;
	void static mf07() volatile;
};

extern int v01;
static volatile int v02;

static void p01();
extern void p02();

static void f01()
{
}

static inline void f02()
{
}
