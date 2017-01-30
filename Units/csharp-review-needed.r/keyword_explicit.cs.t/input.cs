// cs_keyword_explicit.cs
using System;
struct Digit
{
   byte value;
   public Digit(byte value) 
   {
      if (value<0 || value>9) throw new ArgumentException();
      this.value = value;
   }

   // define explicit byte-to-Digit conversion operator:
   public static explicit operator Digit(byte b) 
   {
      Console.WriteLine("conversion occurred");
      return new Digit(b);
   }
}

class Test 
{
   public static void Main() 
   {
      byte b = 3;
      Digit d = (Digit)b; // explicit conversion
   }
}
