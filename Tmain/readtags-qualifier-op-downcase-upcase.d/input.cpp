// ctags -o output.tags --fields=+i input.cpp
class A {
protected:
  int f (void) {
    return 1;
  }
};

class B {
protected:
  int g (void) {
    return 1;
  }
};

class C: A, B {
public:
  int h (void) {
    return f () + g ();
  }
};
