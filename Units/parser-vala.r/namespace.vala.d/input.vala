using Data;

void main(string[] args) {
   var p = new Person();
   var a = new Address();
   a.street = "Oxford Street";
   p.address = a;
   p.name = "John";
   p.age = 21;
   print("Hello %s, you're %d years old\nliving in %s\n", p.name, p.age, p.address.street);
}

namespace Data {
   class Address {
      public string country;
      public string city;
      public string d_street;
      public int building;
      public int floor;
   
      public string street {
         owned get {
            return d_street;
         }
         set {
            d_street = value;
         }
      }
   }
   
   namespace Private {
      class Contact {
         public string email;
         public string phone;
         public string id;
      }
   }
}

class Person {
   public Data.Address address {get; set;}
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
