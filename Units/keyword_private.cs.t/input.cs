// private_keyword.cs
using System;
class Employee 
{
   public string name = "xx";
   double salary = 100.00;   // private access by default
   private int not_visible = 3;  // strangely, not in original example
   public double AccessSalary() {
      return salary;
   }
}

class MainClass 
{
   public static void Main() 
   {
      Employee e = new Employee();

      // Accessing the public field:
      string n = e.name; 
      
      // Accessing the private field:
      double s = e.AccessSalary();    
   }
}
