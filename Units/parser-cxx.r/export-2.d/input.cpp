// Taken from https://en.cppreference.com/w/cpp/language/modules

export module A; // declares the primary module interface unit for named module 'A'

// hello() will be visible by translations units importing 'A'
export char const* hello() { return "hello"; }

// world() will NOT be visible.
char const* world() { return "world"; }

export namespace hi
{
    char const* english() { return "Hi!"; }
    char const* french()  { return "Salut!"; }
}

export enum x { a = 1 };
