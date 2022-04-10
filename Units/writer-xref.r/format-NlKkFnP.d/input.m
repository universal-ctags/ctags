@protocol X
- doSomething;
@end

@interface A: NSObject
- aMethod;
@end

@interface B: A <X>
{
@public
  int i;
@private
  int j;
}
@end

static int x(void)
{
  return 0;
}

int y(void)
{
  return 1;
}

@implementation B
- aMethod
{
  return nil;
}
- doSomething
{
  return self;
}
@end
