class Point {
  double x, y;
};

namespace myns {
  class Point * (g)();
  namespace mysubns {
    class Point * ((h))();
  };
};

class Point * (f)() {
  return nullptr;
}
