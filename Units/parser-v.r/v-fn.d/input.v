fn fn1() {
}

fn fn2(a int) {
}

fn fn3(a int, b string, c Foo) {
}

fn fn4() ! {
}

fn fn5() ?Test {
}

fn fn6() (int, int) {

    fn fn7() {

        fn fn8(a int, b string, c Foo) (!Test, int, Foo) {
        }
    }
}

// receivers

fn (s Foo) fnR1(a int) {
}

fn (s *Foo) fnR2(a int, b string, c Foo) ?Foo {
}

fn main () {

    // anon
    x := fn(a int, b int) int { return a + b }
    y := x(0, 0)
    z := fn() {

        // closures
        a := 0
        b := 0
        add :=
            fn        [a,   b	]   (   c  int  , d  int )	int  {
            return a + b + c + d
        }
    }
}
