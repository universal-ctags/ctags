const (
    // decimal
    a = 20
    b = 999999999
    c = 45.6
    d = 3.14
    e = 1_000_000
    f = 3_122.55
    // strings
    g = "hi"
    h = "hi \"tim\""
    i = '\u2605'
    j = '\xe2\x98\x85'
    k = `x`
    // other bases
    l = 0b1111_0000_1010
    m = -0b1111_0000_1010
    n = 0x00
    o = 0xabcdef
    p = 0xF_F
    q = 0o173
    r = 0o17_3
    // unicode
    s = 'Hello World 👋'
    // exp
    t = 42e1
    u = 123e-2
    v = 456E+2
)

const x = 99
pub const pi = 3.14

const (
    numbers = [1, 2, 3]
    red     = Color{
        r: 255
        g: 0
        b: 0
    }
    green = &Colour{0, 255, 0}
    blue = rgb(0, 0, 255)
    test1 = [
        Aaa{},
        Bbb{3, 4},
    ]
    test2 = test1 + 3
    test3 = 0
)

pub const (
    a = 9 + 3
    b = a[3]
    c = 0
    d = r'hi'
    e = r"there"
)
