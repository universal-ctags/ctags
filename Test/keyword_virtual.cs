// cs_virtual_keyword.cs
// Virtual and override
using System;
class TestClass 
{
   public class Dimensions 
   {
      public const double pi = Math.PI;
      protected double x, y;
      public Dimensions() 
      {
      }
      public Dimensions (double x, double y) 
      {
         this.x = x;
         this.y = y;
      }

      public virtual double Area() 
      {
         return x*y;
      }
   }

   public class Circle: Dimensions 
   {
      public Circle(double r): base(r, 0) 
      {
      }

      public override double Area() 
      { 
         return pi * x * x; 
      }
   }

   class Sphere: Dimensions 
   {
      public Sphere(double r): base(r, 0) 
      {
      }

         public override double Area()
      {
         return 4 * pi * x * x; 
      }
   }

   class Cylinder: Dimensions 
   {
      public Cylinder(double r, double h): base(r, h) 
      {
      }

      public override double Area() 
      {
         return 2*pi*x*x + 2*pi*x*y; 
      }
   }

   public static void Main()  
   {
      double r = 3.0, h = 5.0;
      Dimensions c = new Circle(r);
      Dimensions s = new Sphere(r);
      Dimensions l = new Cylinder(r, h);
      // Display results:
      Console.WriteLine("Area of Circle   = {0:F2}", c.Area());
      Console.WriteLine("Area of Sphere   = {0:F2}", s.Area());
      Console.WriteLine("Area of Cylinder = {0:F2}", l.Area());
   }
}
