// Taken from https://en.cppreference.com/w/cpp/language/modules

///////  A.cpp
export module A;     // primary module interface unit

export import :B;    // Hello() is visible when importing 'A'.
import :C;           // WorldImpl() is now visible only for 'A.cpp'.
// export import :C; // ERROR: Cannot export a module implementation unit.

// World() is visible by any translation unit importing 'A'.
export char const* World()
{
    return WorldImpl();
}
