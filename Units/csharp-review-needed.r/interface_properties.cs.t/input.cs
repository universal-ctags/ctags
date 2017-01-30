// cs_interface_properties.cs
// Interface Properties
using System;
interface IEmployee 
{
   string Name 
   {
      get;
      set;
   }

   int Counter 
   {
      get;
   }
}

public class Employee: IEmployee 
{
   public static int numberOfEmployees;
   private int counter;
   private string name;
   // Read-write instance property:
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
   // Read-only instance property:
   public int Counter 
   {
      get 
      {
         return counter; 
      }
   }
   // Constructor:
   public Employee() 
   {
      counter = ++counter + numberOfEmployees;
   }
}

public class MainClass 
{
   public static void Main() 
   {
      Console.Write("Enter number of employees: ");
      string s = Console.ReadLine();
      Employee.numberOfEmployees = int.Parse(s);
      Employee e1 = new Employee();
      Console.Write("Enter the name of the new employee: ");
      e1.Name = Console.ReadLine();  
      Console.WriteLine("The employee information:");
      Console.WriteLine("Employee number: {0}", e1.Counter);
      Console.WriteLine("Employee name: {0}", e1.Name);
   }
}
