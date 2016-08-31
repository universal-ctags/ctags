namespace ABC {
  int x = 1;
}

int foo (void)
{
  namespace X = ABC;
  return X::x;
}
