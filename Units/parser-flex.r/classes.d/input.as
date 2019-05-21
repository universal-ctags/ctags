package {
  class C1 {
    public function m1():Boolean { return 0; }
  }
  class C2 extends C1 {}
  class C3 {}
  interface I1 {}
  interface I2 {}
  interface I3 extends I1, I2 {}
  interface I4 extends I3 {}
  class C4 implements I1 {}
  class C5 extends C3 implements I1 {}
  class C6 extends C3 implements I1, I2 {}

  dynamic class C7{}
}
