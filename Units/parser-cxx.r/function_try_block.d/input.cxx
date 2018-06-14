/* Taken from
 * http://en.cppreference.com/w/cpp/language/function-try-block */

struct S {
    std::string m;
    S(const std::string& arg) try : m(arg, 100) {
        std::cout << "constructed, mn = " << m << '\n';
    } catch(const std::exception& e) {
        std::cerr << "arg=" << arg << " failed: " << e.what() << '\n';
    } // implicit throw; here
};

int f(int n = 2) try {
   ++n; // increments the function parameter
   throw n;
} catch(...) {
   ++n; // n is in scope and still refers to the function parameter
   assert(n == 4);
   return n;
}

int f2() try {
} catch(SomeKindOfException &f2v1)
{
} catch(SomeOtherKindOfException &f2v2)
{
} catch(...) {
}

int f3()
{
	// This is NOT a function-try-block
	try {
		int f3v1 = 10;
	} catch(std::exception & f3v2)
	{
	}
}

auto f4() -> void try {
} catch(...)
{
}