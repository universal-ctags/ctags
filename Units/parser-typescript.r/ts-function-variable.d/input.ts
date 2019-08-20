function mul(a: number, b: number) {
  return a * b;
}

function numberToString(a: number[]) {
  var stringArray = a.map(v => v.toString());
  return stringArray;
}

function map<T, U>(a: T[], f: (x: T) => U): U[] {
  var result: U[] = [];
  for (var i = 0; i < a.length; i++) result.push(f(a[i]));
  return result;
}

function zip<S, T, U>(x: S[], y: T[], combine: (x: S) => (y: T) => U): U[] {
  var len = Math.max(x.length, y.length);
  var result: U[] = [];
  for (var i = 0; i < len; i++) result.push(combine(x[i])(y[i]));
  return result;
}

function f1(x: string | number | boolean) {
  if (typeof x === "string" || typeof x === "number") {
    var y = x; // Type of y is string | number
  }
  else {
    var z = x; // Type of z is boolean
  }
}

function f2(input: boolean) {
    let a = 100;

    if (input) {
        let b = a + 1;
        return b;
    }

    return a;
}

function f3(input: boolean) {
    const a = 100;

    if (input) {
        let b = a + 1;
        return b;
    }

    return a;
}

function f4(input = [1, 2]) {
  let [first, second] = input;
}

function f5([first, second]: [number, number]) {
  console.log(first);
  console.log(second);
}

function d(p: string): void {
    const view = p as BigTableView;
}
