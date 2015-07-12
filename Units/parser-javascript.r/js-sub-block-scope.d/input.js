
function parent() {
  function foo() {
    if (test) {
      function hello() {
      }
    } else {
      function hi() {
      }
    }
  }

  function bar() {
    function hello2() {}
    function hi2() {}
  }
}
