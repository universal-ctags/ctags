/* Derrived from https://developer.mozilla.org/en-US/docs/Web/JavaScript/Reference/Functions/rest_parameters */
function rf0(a,  b, ...manyMoreArgs0) {
  console.log("a", a)
  console.log("b", b)
  console.log("manyMoreArgs", manyMoreArgs0)
}

function rf1(...manyMoreArgs1) {
  console.log("manyMoreArgs", manyMoreArgs1)
}

function rf2(a,  b, ...
	     manyMoreArgs0) {
  console.log("a", a)
  console.log("b", b)
  console.log("manyMoreArgs", manyMoreArgs0)
}

function rf3(...
	     manyMoreArgs1) {
  console.log("manyMoreArgs", manyMoreArgs1)
}
