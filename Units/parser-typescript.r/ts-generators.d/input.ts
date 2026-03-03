// Taken from MDN
// ==> Rectangle.js <==
class Rectangle {
  height: number;
  width: number;
  constructor(height, width) {
    this.height = height;
    this.width = width;
  }
  // Getter
  get area() {
    return this.calcArea();
  }
  // Method
  calcArea() {
    return this.height * this.width;
  }
  *getSides() {
    yield this.height;
    yield this.width;
    yield this.height;
    yield this.width;
  }
}

// ==> idMaker.js <==
function* idMaker() {
  let index = 0;
  while (true) {
    yield index++;
  }
}
