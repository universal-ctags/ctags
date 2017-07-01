@interface X: NSObject
- doNothing;
- (void)doVoidReturn;
- (void*)doVoidPtrReturn;
- doNothing: idarg;
- doNothing:(id) idargExplicit;
- doNothing: idarg1 with: idarg2;
- doNothing: idarg1 with: (id) idarg2Explicit;
- doNothing: idarg1 and: (id*) idarg2PtrExplicit;
- (void)doVoidReturn: idarg1 with: idarg2;
- (void)doVoidReturn: idarg1 withId: (id) idarg2Explicit;
- (void)doVoidReturn: idarg1 and: (id*) idarg2PtrExplicit;
- (void*)doVoidPtr: idarg1 with: idarg2;
- (void*)doVoidPtr: idarg1 and: (id) idarg2Explicit;
- (void*)doVoidPtr: idarg1 andWith: (id**) idarg2PtrExplicit;
- (void*)doVoidPtr: idarg1 andWithStruct: (struct foo *) idarg2PtrExplicit;
- (void*)doVoidUInt: (unsigned int) idarg1 andWithStruct: (struct foo *) idarg2PtrExplicit;
@end

@implementation X
- doNothing
{
  return nil;
}
- doNothing: idarg
{
  return nil;
}
- (void*)doVoidUInt: (unsigned int) idarg1 andWithStruct: (struct foo *) idarg2PtrExplicit
{
  return NULL;
}
@end
