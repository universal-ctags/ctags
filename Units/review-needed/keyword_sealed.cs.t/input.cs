// cs_sealed_keyword.cs
// Sealed classes
using System;
sealed class MyClass 
{
   public int x; 
   public int y;
}

class MainClass 
{
   public static void Main() 
   {
      MyClass mC = new MyClass(); 
      mC.x = 110;
      mC.y = 150;
      Console.WriteLine("x = {0}, y = {1}", mC.x, mC.y); 
   }
}
