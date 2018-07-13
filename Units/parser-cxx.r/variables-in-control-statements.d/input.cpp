#include <string>
#include <memory>

// Helpers (should be hidden in ctags output)

unsigned short int getUShort()
{
	return 10;
}

int * getPointer()
{
	return 0;
}

int n01;
unsigned short n02;
int * n03;

typedef int INT;

int n04()
{
	return 0;
}

int n05,n06,n07,n08,n09,n10,n11,n12,n13,n14,n15,n16;

template<typename T> func(int no){ return no; };

// The real test starts here

void test()
{
	// Valid variable declarations inside a for loop control structure
	for(int f01=0;f01<10;f01++)
	{
	}

	for(int f02 = getUShort(), * f03 = getPointer();f03 < (getPointer() + 10);f03++)
	{
	}

	for(int f04=0,f05=5,f06=10;f04<10;f04++)
	{
	}

	for(std::string f07 = "test";;)
	{
	}

	for(int f08 : { 1,2,3,4,5 })
	{
	}

	for(std::unique_ptr<int> f09 = 0;;)
	{
	}

	for(std::unique_ptr<int> f10(getPointer());;)
	{
	}

	for(std::pair<int,int> f11;;)
	{
	}

	for(auto f12 : { 't','e','s','t' })
	{
	}

	// Non valid variable declarations inside for
	for(;;)
	{
	}

	for(n01 = 0;n01 < 10;n01++)
	{
	}

	for(n02 = getUShort(), n03 = getPointer();;)
	{
	}

	for(n04();n04();)
	{
	}

	// Valid variable declaration inside an if
	if(int i01 = 10 + 20)
	{
	}

	if(INT * i02 = 0)
	{
	}

	if(INT & i03 = n01)
	{
	}

	// non valid declarations inside if
	if(n05 & n06)
	{
	}

	if(n07 * n08)
	{
	}

	if(n09 && n10)
	{
	}

	if(n11 < n12 || n13 > n14)
	{
	}

	if(static_cast<short>(n15) < n14)
	{
	}

	if(func<int>(n16))
	{
	}

	// Valid variable declarations inside a while
	while(unsigned short int w01 = getUShort())
	{
	}
}