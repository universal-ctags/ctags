// keyword_class.cs
// class example
using System;
public class Kid 
{
   private int age;
   private string name;

   // Default constructor:
   public Kid() 
   {
      name = "N/A";
   }

   // Constructor:
   public Kid(string name, int age) 
   {
      this.name = name;
      this.age = age;
   }

   // Printing method:
   public void PrintKid() 
   {
      Console.WriteLine("{0}, {1} years old.", name, age);
   }
}

public class MainClass 
{
   public static void Main() 
   {
      // Create objects
      // Objects must be created using the new operator:
      Kid kid1 = new Kid("Craig", 11);
      Kid kid2 = new Kid("Sally", 10);

      // Create an object using the default constructor:
      Kid kid3 = new Kid(); 

      // Display results:
      Console.Write("Kid #1: ");
      kid1.PrintKid();
      Console.Write("Kid #2: ");
      kid2.PrintKid();
      Console.Write("Kid #3: ");
      kid3.PrintKid();
   }
}
