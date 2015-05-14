// abstract_keyword.cs
// Abstract Classes
using System;
abstract class MyBaseC   // Abstract class
{
   protected int x = 100; 
   protected int y = 150;
   public abstract void MyMethod();   // Abstract method

   public abstract int GetX   // Abstract property
   {
      get;
   }

   public abstract int GetY   // Abstract property
   {
      get;
   }
}

class MyDerivedC: MyBaseC
{
   public override void MyMethod() 
   {
      x++;
      y++;   
   }   

   public override int GetX   // overriding property
   {
      get 
      {
         return x+10;
      }
   }

   public override int GetY   // overriding property
   {
      get
      {
         return y+10;
      }
   }

   public static void Main() 
   {
      MyDerivedC mC = new MyDerivedC();
      mC.MyMethod();
      Console.WriteLine("x = {0}, y = {1}", mC.GetX, mC.GetY);    
   }
}
