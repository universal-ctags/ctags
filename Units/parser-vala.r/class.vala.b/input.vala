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

class Address {
   string country;
   string city;
   string street;
   int building;
   int floor;
}

class Person {
   public Address address {get; set;}
   public string name {get; set;}
   private int d_age;

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
}
