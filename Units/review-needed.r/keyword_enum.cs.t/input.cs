// C# Programmer's Reference: enum
// keyword_enum.cs
// enum initialization:
using System;
public class EnumTest 
{
   enum Days {Sat=1, Sun, Mon, Tue, Wed, Thu, Fri};

   public static void Main() 
   {
      int x = (int) Days.Sun;
      int y = (int) Days.Fri;
      Console.WriteLine("Sun = {0}", x);
      Console.WriteLine("Fri = {0}", y);
   }
}

// keyword_enum2.cs
// Using long enumerators
using System;
public class EnumTest 
{
   enum Range :long {Max = 2147483648L, Min = 255L};
   public static void Main() 
   {
      long x = (long) Range.Max;
      long y = (long) Range.Min;
      Console.WriteLine("Max = {0}", x);
      Console.WriteLine("Min = {0}", y);
   }
}
