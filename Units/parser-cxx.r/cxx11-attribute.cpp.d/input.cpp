// Tkane from https://en.cppreference.com/w/cpp/language/attributes
[[gnu::always_inline]] [[gnu::hot]] [[gnu::const]] [[nodiscard]]
inline int f(); // declare f with four attributes
 
[[gnu::always_inline, gnu::const, gnu::hot, nodiscard]]
int g(); // same as above, but uses a single attr specifier that contains four attributes
 
// C++17:
[[using gnu : const, always_inline, hot]] [[nodiscard]]
int h[[gnu::always_inline]](); // an attribute may appear in multiple specifiers
 
int i() { return 0; }
 

/* Taken from issue #2364 opened by andrejlevkovitch. */
// main.cpp
#include <cstdlib>
#include <iostream>

void foo();

int main([[maybe_unused]]int argc, [[maybe_unused]]char *argv[]) {
  int alpha;
  int bravo;
  int charlie;

  return EXIT_SUCCESS;
}
