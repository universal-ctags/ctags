// property.cs
// Properties
using System;
public class Employee 
{
   public static int numberOfEmployees;
   private static int counter;
   private string name;

   // A read-write instance property:
   public string Name 
   {
      get 
      {
         return name; 
      }
      set 
      {
         name = value; 
      }
   }

   // A read-only static property:
   public static int Counter 
   {
      get 
      {
         return counter; 
      }
   }

   // Constructor:
   public Employee() 
   {
      // Calculate the employee's number:
      counter = ++counter + numberOfEmployees;
   }
}

public class MainClass
{
   public static void Main() 
   {
      Employee.numberOfEmployees = 100;
      Employee e1 = new Employee();
      e1.Name = "Claude Vige";  
      Console.WriteLine("Employee number: {0}", Employee.Counter);
      Console.WriteLine("Employee name: {0}", e1.Name);
   }
}
