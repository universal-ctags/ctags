// keyword_struct.cs
// struct declaration and initialization
using System;
public struct Point 
{
   public int x, y;

   public Point(int p1, int p2) 
   {
      x = p1;
      y = p2;    
   }
}

class MainClass 
{
   public static void Main()  
   {
      // Initialize:   
      Point myPoint = new Point();
      Point yourPoint = new Point(10,10);

      // Display results:
      Console.Write("My Point:   ");
      Console.WriteLine("x = {0}, y = {1}", myPoint.x, myPoint.y);
      Console.Write("Your Point: ");
      Console.WriteLine("x = {0}, y = {1}", yourPoint.x, yourPoint.y);
   }
}
