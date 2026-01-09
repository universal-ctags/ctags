enum e {
  x = [] {
    using T3 = int;
    return 1;
  }()
};


int main(void)
{
  return e::x;
}
