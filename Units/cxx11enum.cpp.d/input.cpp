
enum A:int {A_a, A_b, A_c};
enum B:long {B_a, B_b, B_c};
enum C:unsigned int {C_a, C_b, C_c};

class Foo {
  enum D:int {a, b, c};
  virtual void foo(enum D a);
};
