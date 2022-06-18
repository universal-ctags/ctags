// Taken from #3390 submitted by @delsner
#include <iostream>
#include <vector>

template<typename T>
class Stack
{
private:
  std::size_t size;
  std::vector<T> stack;

public:
  ~Stack(){};

  // Not recognized by ctags.
  Stack<T>(std::size_t n) {
    size = n;
    std::cout << "Constructing a `Stack<T>`\n";
  }
};

int main() {
  Stack<float> sf{2};
  return 0;
}
