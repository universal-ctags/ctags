// General Structure of a C# Program
// A skeleton of a C# program 
using System;
namespace MyNamespace1 
{
   class MyClass1 
   {
   }
   struct MyStruct 
   {
   }
   interface IMyInterface 
   {
   }
   delegate int MyDelegate();
   enum MyEnum 
   {
   } 
   namespace MyNamespace2 
   {
   }
   class MyClass2 
   {
      public static void Main(string[] args) 
      {
      }
   }
}


// Using Fully Qualified Names
namespace N1     // N1
{
   class C1      // N1.C1
   {
      class C2   // N1.C1.C2
      {
      }
   }
   namespace N2  // N1.N2
   {
      class C2   // N1.N2.C2
      {
      }
   }
}
