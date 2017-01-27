// cs_out.cs 
using System;
public class MyClass 
{
   public static int TestOut(out char i) 
   {
      i = 'b';
      return -1;
   }

   public static void Main() 
   {
      char i;   // variable need not be initialized
      Console.WriteLine(TestOut(out i));
      Console.WriteLine(i);
   }
}
