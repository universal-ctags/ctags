// protected_public.cs
// Public access
using System;
class MyClass1 
{
   public int x; 
   public int y;
}

class MyClass2 
{
   public static void Main() 
   {
      MyClass1 mC = new MyClass1();

      // Direct access to public members:
      mC.x = 10;
      mC.y = 15;
      Console.WriteLine("x = {0}, y = {1}", mC.x, mC.y); 
   }
}
