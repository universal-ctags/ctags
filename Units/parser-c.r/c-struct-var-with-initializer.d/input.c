/* Derived from an issue (#1311) opened by BodoStroesser */

struct {
  int a;
} b = { 1 };

int c;

int d(int e) {
  struct {
    int f;
  } g = { 1 };
  int h;
  g = (typeof(g)){ 2 };
}
