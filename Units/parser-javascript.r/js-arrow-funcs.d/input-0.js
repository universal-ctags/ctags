function a() {
  function b() {
    let x = 42;
    return x;
  }
  let y = b();
  return y
}

function c() {
  let d = () => {
    return 42;
  }

  function d2() {
    return 1;
  }
}

let e = () => {
  function f() {

  }
}

let g = () => {
  let h = () => {
    return 42;
  }
  let i = x => {
    return x * 42;
  }
  let j = () => {
    return 42;
  }
}

function k() {}
