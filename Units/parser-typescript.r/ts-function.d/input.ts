function point(x: number, y: number): Point {
  return { x: x, y: y };
}

export function equals(p1: Point, p2: Point) {
  return p1.x == p2.x && p1.y == p2.y;
}

export function message(s: string) {
  console.log(s);
}

export default function point(x: number, y: number) {
  return { x, y };
}

function numberToString(a: number[]) {
  var stringArray = a.map(v => v.toString());
  return stringArray;
}

function *g(): Iterable<string> {
  for (var i = 0; i < 100; i++) {
    yield ""; // string is assignable to string
  }
  yield * otherStringGenerator(); // otherStringGenerator must be iterable and element type assignable to string
}

function *h(limit) {
  for (var i = 0; i < limit; i++) {
    yield i;
  }
}

async function fn(): Promise<number> {  
  var i = await p; // suspend execution until 'p' is settled. 'i' has type "number"  
  return 1 + i;  
}

function isCat(a: Animal): a is Cat {
  return a.name === 'kitty';
}

var foo = function() {
  console.log();
};

var foo2 = () => {
  console.log();
};
