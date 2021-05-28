typedef char muchar;

struct Range {
  Range (int b, int e): begin(b), end(end) {}
  int begin, end;
};

class Char {
public:
  Char (char i = 0): val(i), r(i, i) {}

  operator volatile muchar() const { return val; }
  operator volatile const muchar*() const { return 0; }
  operator volatile const muchar*const() const { return 0; }
  operator volatile int *() const { return 0; }
  operator volatile int *volatile() const { return 0; }
  operator int *volatile() const { return 0; }
  operator volatile const int *() const { return 0; }
  operator volatile const int *volatile() const { return 0; }
  operator int *volatile const() const { return 0; }

  operator volatile const int *() volatile { return 0; }
  operator volatile const int *volatile() volatile { return 0; }
  operator int *volatile const() volatile { return 0; }

  operator volatile struct Range *() const { return 0; }
  operator volatile struct Range *volatile() const { return 0; }
  operator struct Range *volatile() const { return 0; }
  
  operator volatile const struct Range *() const { return 0; }
  operator volatile const struct Range *volatile() const { return 0; }
  operator struct Range *volatile const() const { return 0; }

  operator volatile const struct Range *() volatile { return 0; }
  operator volatile const struct Range *volatile() volatile { return 0; }
  operator struct Range *volatile const() volatile { return 0; }      
  
private:
  int val;
  struct Range r;
};

int main(void)
{
  return Char ();
}
