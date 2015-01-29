
enum A:int {A_a, A_b, A_c};
enum B:long {B_a, B_b, B_c};
enum C:unsigned int {C_a, C_b, C_c};
enum class E1:short {E1_a, E1_b, E1_c};

class Foo {
  enum D:int {a, b, c};
  enum class E2:short {E2_a, E2_b, E2_c};
  virtual void foo(enum D a);
};
