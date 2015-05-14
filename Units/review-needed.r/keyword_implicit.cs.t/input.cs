// cs_keyword_implicit.cs
using System;
struct Digit
{
   byte value;

   public Digit(byte value) 
   {
      if (value < 0 || value > 9) throw new ArgumentException();
      this.value = value;
   }

   // define implicit Digit-to-byte conversion operator:
   public static implicit operator byte(Digit d) 
   {
      Console.WriteLine( "conversion occurred" );
      return d.value;
   }
}

class Test 
{
   public static void Main() 
   {
      Digit d = new Digit(3);

      // implicit (no cast) conversion from Digit to byte
      byte b = d;   
   }
}
