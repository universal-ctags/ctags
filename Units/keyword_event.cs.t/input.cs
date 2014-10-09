// event_keyword.cs
using System;
public delegate void MyDelegate();   // delegate declaration

public interface I 
{
   event MyDelegate MyEvent;
   void FireAway();
}

public class MyClass: I 
{
   public event MyDelegate MyEvent;

   public void FireAway() 
   {
      if (MyEvent != null)
         MyEvent();
   }
}

public class MainClass 
{
   static private void f() 
   {
      Console.WriteLine("This is called when the event fires.");
   }

   static public void Main () 
   {
      I i = new MyClass();

      i.MyEvent += new MyDelegate(f);
      i.FireAway();
   }
}

// event_keyword2.cs
using System;
using System.Collections;

public delegate void MyDelegate1(int i);
public delegate void MyDelegate2(string s);
public delegate void MyDelegate3(int i, object o);
public delegate void MyDelegate4();

public class PropertyEventsSample 
{
   private Hashtable eventTable = new Hashtable();

   public event MyDelegate1 Event1 
   {
      add 
      {
         eventTable["Event1"] = (MyDelegate1)eventTable["Event1"] + value;
      }
      remove
      {
         eventTable["Event1"] = (MyDelegate1)eventTable["Event1"] - value; 
      }
   }

   public event MyDelegate1 Event2 
   {
      add 
      {
         eventTable["Event2"] = (MyDelegate1)eventTable["Event2"] + value;
      }
      remove
      {
         eventTable["Event2"] = (MyDelegate1)eventTable["Event2"] - value; 
      }
   }

   public event MyDelegate2 Event3 
   {
      add 
      {
         eventTable["Event3"] = (MyDelegate2)eventTable["Event3"] + value;
      }
      remove
      {
         eventTable["Event3"] = (MyDelegate2)eventTable["Event3"] - value; 
      }
   }

   public event MyDelegate3 Event4 
   {
      add 
      {
         eventTable["Event4"] = (MyDelegate3)eventTable["Event4"] + value;
      }
      remove
      {
         eventTable["Event4"] = (MyDelegate3)eventTable["Event4"] - value; 
      }
   }

   public event MyDelegate3 Event5 
   {
      add 
      {
         eventTable["Event5"] = (MyDelegate3)eventTable["Event5"] + value;
      }
      remove
      {
         eventTable["Event5"] = (MyDelegate3)eventTable["Event5"] - value; 
      }
   }

   public event MyDelegate4 Event6 
   {
      add 
      {
         eventTable["Event6"] = (MyDelegate4)eventTable["Event6"] + value;
      }
      remove
      {
         eventTable["Event6"] = (MyDelegate4)eventTable["Event6"] - value; 
      }
   }
}

public class MyClass 
{
   public static void Main() 
   {
   }
}

// event_keyword3.cs
using System;

public delegate void MyDelegate1();

public interface I1 
{
   event MyDelegate1 MyEvent;
}

public delegate int MyDelegate2(string s);

public interface I2 
{
   event MyDelegate2 MyEvent;
}

public class ExplicitEventsSample: I1, I2 
{
   public event MyDelegate1 MyEvent;  // normal implementation of I1.MyEvent.

   event MyDelegate2 I2.MyEvent   // explicit implementation of I2.MyEvent
   {
      add
      {
         MyEvent2Storage += value;
      }
      remove
      {
         MyEvent2Storage -= value;
      }
   }

   private MyDelegate2 MyEvent2Storage;  // underlying storage for I2.MyEvent.

   private void FireEvents() 
   {
      if (MyEvent != null)
         MyEvent();
      if (MyEvent2Storage != null)
         MyEvent2Storage("hello");
   }
}

public class MyClass 
{
   public static void Main() 
   {
   }
}
