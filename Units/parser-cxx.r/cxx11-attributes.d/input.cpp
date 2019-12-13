

[[deprecated("does nothing")]] [[gnu::always_inline, noreturn]] void deprecated()
{
	
}

int main([[maybe_unused]] int argc,[[maybe_unused]] char ** argv)
{
	int v1;
	
	if(1)
	{
		[[likely]];
		int v2;
	}
	
	switch(argc)
	{
		case 1:
			[[fallthrough]];
		[[likely]] case 2:
		{
			int v3;
			return 1;
		}
		[[unlikely]] case 3:
		{
			int v4;
			return 2;
		}
	}

	return 0;
}
