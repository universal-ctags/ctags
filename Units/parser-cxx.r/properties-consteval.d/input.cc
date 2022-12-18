// Taken from https://en.cppreference.com/w/cpp/language/consteval
consteval int sqr(int n)
{
    return n*n;
}
constexpr int r = sqr(100); // OK
 
int x = 100;
int r2 = sqr(x);            // Error: Call does not produce a constant
 
consteval int sqrsqr(int n)
{
    return sqr(sqr(n));     // Not a constant expression at this point, but OK
}
 
constexpr int dblsqr(int n)
{
    return 2 * sqr(n);      // Error: Enclosing function is not consteval
                            // and sqr(n) is not a constant
}

consteval int f() { return 42; }
consteval auto g() { return &f; }
consteval int h(int (*p)() = g()) { return p(); }

