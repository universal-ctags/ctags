module x

struct Foo {
    a int
    b Foo
mut:
    c map[int]Foo
pub:
    d []int
pub mut:
    e u8 [required]
__global:
    f ?[]Foo
    g []?Foo
}
