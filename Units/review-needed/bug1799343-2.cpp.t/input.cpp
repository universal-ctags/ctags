class P {
  protected:
   int x;
};

namespace A {
   class P {
  protected:
     int x;
   };
   class Q {
     int y;
   };

   namespace C {
     class R: ::P, A::Q {
       int z;
       int f (int v) { return v + x; }
     };
   }
}

namespace B {
   class S : A::C::R {
     int t;
   };
}
