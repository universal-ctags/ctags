module aaa

interface Foo {
    id string
    foo &Foo
    fn1(a int, b int, c int)
    fn2() !&Test
}
