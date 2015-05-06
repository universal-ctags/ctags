// Programmer's Reference: Passing Array Using ref and out
public static void MyMethod(out int[] arr) 
{
   arr = new int[10];   // definite assignment of arr
}

public static void MyMethod(ref int[] arr) 
{
   arr = new int[10];   // arr initialized to a different array
}

using System; 
class TestOut 
{
   static public void FillArray(out int[] myArray) 
   {
      // Initialize the array:
      myArray = new int[5] {1, 2, 3, 4, 5};
   }

   static public void Main() 
   {
      int[] myArray; // Initialization is not required

      // Pass the array to the callee using out:
      FillArray(out myArray);

      // Display the array elements:
      Console.WriteLine("Array elements are:");
      for (int i=0; i < myArray.Length; i++)
         Console.WriteLine(myArray[i]);
   }
}
