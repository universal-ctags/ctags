// Taken from https://doc.rust-lang.org/reference/const_eval.html#const-functions
const fn square(x: i32) -> i32 { x * x }

const VALUE: i32 = square(12);
