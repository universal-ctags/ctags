fn main () {
    a := 0
    mut b := 0
    mut c, d := foo()
    e, mut f := foo()
    x, x = foo()
    x, x, x = a, b, c
    g, h, i, mut j, k := x, x, x, x, x
    l = unsafe{ nil }
}
