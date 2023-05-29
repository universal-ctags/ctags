module aaa

interface Foo {
    id string
    foo &Foo
    fn1()
    fn2() fn2.Test
    fn3(a int, b int, c int) !&Test
    fn4() [][]a.b.c.D
    fn5()
}
