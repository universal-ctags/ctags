// protected_keyword.cs
using System;
class MyClass 
{
   protected int x; 
   protected int y;
}

class MyDerivedC: MyClass 
{
   public static void Main() 
   {
      MyDerivedC mC = new MyDerivedC();

      // Direct access to protected members:
      mC.x = 10;
      mC.y = 15;
      Console.WriteLine("x = {0}, y = {1}", mC.x, mC.y); 
   }
}
