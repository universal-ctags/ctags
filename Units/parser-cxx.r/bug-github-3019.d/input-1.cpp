typedef int myint;

struct Point {
  Point (int i): x(i), y(i) {}
  int x,y;
};

class Int {
public:
  Int (int i = 0): val(i), p(i) {}

  operator myint() const { return val; }
  operator const myint() const { return val; }
  operator myint*() const { return NULL; }
  operator const myint*() const { return NULL; }

  operator myint*const() const { return NULL; }
  operator const myint*const() const { return NULL; }
  
  operator int() const { return val; }

  operator int *() const { return 0; }
  operator const int *() const { return 0; }
  operator const int *const() const { return 0; }
  operator int *const() const { return 0; }

  operator struct Point *() const { return (Point *)&p; }
  operator const struct Point *() const { return 0; }
  operator const struct Point *const() const { return 0; }
  operator struct Point *const() const { return 0; }

private:
  int val;
  struct Point p;
};

int main(void)
{
  return Int ();
}
