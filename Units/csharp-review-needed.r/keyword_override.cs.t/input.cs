// cs_override_keyword.cs
// Calling overriden methods from the base class
using System;
class TestClass 
{
   public class Square 
   {
      public double x;

      // Constructor:
      public Square(double x) 
      {
         this.x = x;
      }

      public virtual double Area() 
      {
         return x*x; 
      }
   }

   class Cube: Square 
   {
      // Constructor:
      public Cube(double x): base(x) 
      {
      }

      // Calling the Area base method:
      public override double Area() 
      {
         return (6*(base.Area())); 
      }
   }

   public static void Main()
   {
      double x = 5.2;
      Square s = new Square(x);
      Square c = new Cube(x);
      Console.WriteLine("Area of Square = {0:F2}", s.Area());
      Console.WriteLine("Area of Cube = {0:F2}", c.Area());
   }
}
