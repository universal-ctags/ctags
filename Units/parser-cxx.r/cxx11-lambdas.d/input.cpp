int test()
{
	auto lambda1 = [](auto a,auto && b)
	{
		int x = 10;
		return a < b + x;
	};
	
	[=]() -> int {
		return [a,b](int c)
		{
			int y = 0;
			return y;
		}
	};
	
	call(10,[lambda1](int a,int b) -> int { return -1; });
}
