/*
 * vala HelloWorld
 *
 * $ valac input.vala 
 * $ ./input 
 * Hello John, you're 21 years old
 */
void main(string[] args) {
    var p = new Person();
    p.name = "John";
    p.age = 21;
    print("Hello %s, you're %d years old\n", p.name, p.age);
}

public class Address {
   string country;
   public string city;
   protected string street;
   internal int building;
   private int floor;
}

class Person {
   public Address address {get; set;}
   public string name {get; set;}
   private int d_age;

   static int population;

   public int age {
      get { return d_age;}
      set { 
         if (value > 0) {
            d_age = value;
         } else {
            d_age = 0;
         }
      }
   }

   int getLastAge (int n) throws GLib.Error { return d_age - n; }

   string [] table = {"a", "b"};
}
