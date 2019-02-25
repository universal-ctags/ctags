// Derived from https://developer.mozilla.org/en-US/docs/Web/JavaScript/Reference/Operators/new

function Car(make, model, year) {
  this.make = make;
  this.model = model;
  this.year = year;
}

var container = {}
container.car = Car;

var car1 = new container.car('Eagle', 'Talon TSi', 1993);

console.log(car1.make);
// expected output: "Eagle"
