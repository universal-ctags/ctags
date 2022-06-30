int d()
{
  class D {
    int i;
  public:
    D( int j,...):i(j){}
    int f(){ return i; }
  } dx{1};

  class E {
    int j;
  public:
    E( int k,...):j(k){}
    int f(){ return j; }
  } e0 {1, 2, 30}, e1 {1}, e2 = {1, 2, 30}, e3 = {1};

  return 0;
}
