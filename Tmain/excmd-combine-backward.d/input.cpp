static int
bar(void)
{
  const int foo = 0;

  return foo;
}

int
main(void)
{
  int foo;
  foo = bar();
  return foo;
}
