
var a = [];
var b = [1, 2, 3];
var c = [
  { a: "hello", b: 42 },
  { a: "hi", b: 41 }
];
var d = [
  [1, 2],
  [3, 4],
  [5, 6],
  [7, [8, 9]]
];

var class = function() {
  this.test1 = {
    foo: [ 1, 2, 3],
    bar: [ 4, 5, 9]
  };
  // FIXME: no tag is generated for test2
  this.test2 = [
    { a: {}, b: {} },
    { a: {}, b: {} }
  ];
  this.test3 = function() {}
}
