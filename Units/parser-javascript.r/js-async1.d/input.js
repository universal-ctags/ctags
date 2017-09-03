/* https://developer.mozilla.org/en-US/docs/Web/JavaScript/Reference/Statements/async_function#Simple_example */

function resolveAfter2Seconds(x) {
  return new Promise(resolve => {
    setTimeout(() => {
      resolve(x);
    }, 2000);
  });
}

async function add1(x) {
  var a = resolveAfter2Seconds(20);
  var b = resolveAfter2Seconds(30);
  return x + await a + await b;
}

add1(10).then(v => {
  console.log(v);  // prints 60 after 2 seconds.
});

async function add2(x) {
  var a = await resolveAfter2Seconds(20);
  var b = await resolveAfter2Seconds(30);
  return x + a + b;
}

add2(10).then(v => {
  console.log(v);  // prints 60 after 4 seconds.
});
