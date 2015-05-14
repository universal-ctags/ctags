// Ordering catch clauses
using System;
class MyClass 
{
   public static void Main() 
   {
      MyClass x = new MyClass();
      try 
      {
         string s = null;
         x.MyFn(s);
      }

      // Most specific:
      catch (ArgumentNullException e) 
      {
         Console.WriteLine("{0} First exception caught.", e);
      }

      // Least specific:
      catch (Exception e) 
      {
         Console.WriteLine("{0} Second exception caught.", e);
      }

   }

   public void MyFn(string s) 
   {
      if (s == null) 
         throw new ArgumentNullException();
   }   
}
