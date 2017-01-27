// cs_params.cs
using System;
public class MyClass 
{

   public static void UseParams(params int[] list) 
   {
      for ( int i = 0 ; i < list.Length ; i++ )
         Console.WriteLine(list[i]);
      Console.WriteLine();
   }

   public static void UseParams2(params object[] list) 
   {
      for ( int i = 0 ; i < list.Length ; i++ )
         Console.WriteLine((object)list[i]);
      Console.WriteLine();
   }

   public static void Main() 
   {
      UseParams(1, 2, 3);
      UseParams2(1, 'a', "test"); 

      int[] myarray = new int[3] {10,11,12};
      UseParams(myarray);
   }
}
