// The parentheses of decltype may contain any expression.
int a, b;
void f1(decltype(a + b) x) {}
void f2(int y, decltype(a * 2) z);
void f3(decltype(a) x, decltype(b - 1) y) {}
decltype(a + b) g(int q) { return 0; }
