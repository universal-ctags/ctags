// keyword_delegate.cs
// delegate declaration
delegate void MyDelegate(int i);

class Program
{
   public static void Main()
   {
      TakesADelegate(new MyDelegate(DelegateFunction));
   }

   public static void TakesADelegate(MyDelegate SomeFunction)
   {
      SomeFunction(21);
   }
   
   public static void DelegateFunction(int i)
   {
      System.Console.WriteLine("Called by delegate with number: {0}.", i);
   }
}
//
// keyword_delegate2.cs
// Calling both static and instance methods from delegates
using System;

// delegate declaration
delegate void MyDelegate();

public class MyClass 
{
   public void InstanceMethod() 
   {
      Console.WriteLine("A message from the instance method."); 
   }

   static public void StaticMethod() 
   {
      Console.WriteLine("A message from the static method.");
   }
}

public class MainClass 
{
   static public void Main() 
   {
      MyClass p = new MyClass();

      // Map the delegate to the instance method:
      MyDelegate d = new MyDelegate(p.InstanceMethod);
      d();

      // Map to the static method:
      d = new MyDelegate(MyClass.StaticMethod);
      d();
   }
}
