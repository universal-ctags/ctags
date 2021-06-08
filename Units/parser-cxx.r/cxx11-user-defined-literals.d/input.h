// Quoted from https://en.cppreference.com/w/cpp/language/user_literal:

// used for side-effects
void operator"" _print ( const char* str )
{
    std::cout << str << '\n';
}

float operator ""_e(const char*)
{
	return 0.0;
}
