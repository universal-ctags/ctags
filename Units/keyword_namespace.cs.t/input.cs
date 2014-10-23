// cs_namespace_keyword.cs
using System;
namespace SomeNameSpace
{
   public class MyClass 
   {
      public static void Main() 
      {
         Nested.NestedNameSpaceClass.SayHello();
      }
   }

   namespace Nested   // a nested namespace
   {
      public class NestedNameSpaceClass 
      {
         public static void SayHello() 
         {
            Console.WriteLine("Hello");
         }
      }
   }
}

// namespace can nest multiple levels at once.
namespace SomeNameSpace.Nested
{
	public class AlternativeNestedNameSpaceClass
	{
	}
}
