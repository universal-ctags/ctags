#include <iostream>
struct C { int x; };
struct D : ::C {
 D() { x = 123; }
 ~D() { std::cout << x << std::endl; }
};
int main(void) {
 D d;
 return 0;
}
