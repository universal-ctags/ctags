
enum A:int {A_a, A_b, A_c};
enum B:long {B_a, B_b, B_c};
enum C:unsigned int {C_a, C_b, C_c};
enum class EC1:short {EC1_a, EC1_b, EC1_c};
enum struct ES1:unsigned {ES1_a, ES1_b, ES1_c};

class Foo {
  enum D:int {a, b, c};
  enum class EC2:unsigned {EC2_a, EC2_b, EC2_c};
  enum struct ES2:int {ES2_a, ES2_b, ES2_c};
  virtual void foo(enum D a);
};
