// Tkane from https://en.cppreference.com/w/cpp/language/attributes
[[gnu::always_inline]] [[gnu::hot]] [[gnu::const]] [[nodiscard]]
inline int f(); // declare f with four attributes

[[gnu::always_inline, gnu::const, gnu::hot, nodiscard]]
int g(); // same as above, but uses a single attr specifier that contains four attributes

// C++17:
[[using gnu : const, always_inline, hot]] [[nodiscard]]
int h[[gnu::always_inline]](); // an attribute may appear in multiple specifiers

int i() { return 0; }

[ [ deprecated ] ] int j(int k) {
	switch(k)
	{
		case 1:
			[[fallthrough]];
		case 2:
			[[likely]]
			return 3;
	}
	
	int v1;
	
	return -1;
}

/* Taken from issue #2364 opened by andrejlevkovitch. */

void foo();

int main([[maybe_unused]]int argc, [[maybe_unused]]char *argv[]) {
  int alpha;
  int bravo;
  int charlie;

  return 0;
}
