fn main () {
    a := 0
    mut b := 0
    mut c, d := foo()
    e, mut f := foo()
    x, x = foo()
    x, x, x = a, b, c
    g, h, i, mut j, k := x, x, x, x, x
    l = unsafe{ nil }
    m := chan int{cap: 1}
}

fn foo(ch chan int) {
    ch <- 123.456
    n := <-ch or {
        println('channel has been closed')
    }
    foo(|x| x.bob())
    x := |a, b| a + b
}
