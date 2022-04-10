struct S{
  int i;
};

typedef S T;

static T t0;
static struct S t0;

int func (T x)
{
  return 0;
}
