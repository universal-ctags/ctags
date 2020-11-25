enum Color {
  Red;
  Green;
  Blue;
  Rgb(r:Int, g:Int, b:Int);
}

interface Printable {
    public function toString():String;
}

typedef User = {
  var age : Int;
  var name : String;
}

class Main {
  static var member:String = "bar";

  static public function main():Void {
    trace("Hello World");
  }
}