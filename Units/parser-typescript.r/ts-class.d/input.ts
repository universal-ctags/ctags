class CPoint {
  x: number;
  y: number;
  constructor(x: number, y: number) {
    this.x = x;
    this.y = y;
  }
}

class BankAccount {
  balance = 0;
  deposit(credit: number) {
    this.balance += credit;
    return this.balance;
  }
}

class CheckingAccount extends BankAccount {
  constructor(balance: number) {
    super(balance);
  }
  writeCheck(debit: number) {
    this.balance -= debit;
  }
}

class List<T extends NamedItem> {
  next: List<T> = null;
  constructor(public item: T) {
  }
  insertAfter(item: T) {
    var temp = this.next;
    this.next = new List(item);
    this.next.next = temp;
  }
  log() {
    console.log(this.item.name);
  }
  // ...
}

class C {
  x: number;
  static x: string;
}

class Messenger {
  message = "Hello World";
  start() {
    var _this = this;
    setTimeout(function() { alert(_this.message); }, 3000);
  }
};

class D {
  data: string | string[];
  getData() {
    var data = this.data;
    return typeof data === "string" ? data : data.join(" ");
  }
}

class Point {
  protected fakePointBuilder: () => { x: number, y: number };
  constructor(public x: number, public y: number) { }
  public length() { return Math.sqrt(this.x * this.x + this.y * this.y); }
  static origin = new Point(0, 0);
}
