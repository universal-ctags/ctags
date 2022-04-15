// Modified from
// https://developer.mozilla.org/en-US/docs/Web/JavaScript/Reference/Functions/get
var obj = {
  log: ['a', 'b', 'c'],
  get() {
    if (this.log.length == 0) {
      return undefined;
    }
    return this.log[this.log.length - 1];
  }
}

// Modified from
// https://developer.mozilla.org/en/docs/JavaScript/Reference/Operators/set
var o = {
  set(str) {
    this.log[this.log.length] = str;
  },
  log: []
}
