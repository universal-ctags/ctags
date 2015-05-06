// cs_static_keyword.cs
// Static members
using System;
public class Employee 
{
   public string id;
   public string name;

   public Employee () 
   {
   }

   public Employee (string name, string id) 
   {
      this.name = name;
      this.id = id;
   } 

   public static int employeeCounter;

   public static int AddEmployee() 
   {
      return ++employeeCounter;
   }
}

class MainClass: Employee 
{
   public static void Main() 
   {
      Console.Write("Enter the employee's name: ");
      string name = Console.ReadLine();
      Console.Write("Enter the employee's ID: ");      
      string id = Console.ReadLine();
      // Create the employee object:
      Employee e = new Employee (name, id);
      Console.Write("Enter the current number of employees: ");
      string n = Console.ReadLine();
      Employee.employeeCounter = Int32.Parse(n);
      Employee.AddEmployee();
      // Display the new information:
      Console.WriteLine("Name: {0}", e.name);
      Console.WriteLine("ID:   {0}", e.id);
      Console.WriteLine("New Number of Employees: {0}",
                         Employee.employeeCounter);
   }
}
