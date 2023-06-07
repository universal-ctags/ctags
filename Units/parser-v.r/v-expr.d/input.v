// not even consts!
fn test_exprs() {
    a := 0
    x = x + x + x
    b := 0
    x = x - x - x
    c := 0
    x = x * x * x
    d := 0
    x = x / x / x
    e := 0
    x = x % x % x
    f := 0
    x = x ^ x
    g := 0
    // assignment
    x += x
    h := 0
    x -= x
    i := 0
    x /= x
    j := 0
    x *= x
    k := 0
    x %= x
    l := 0
    x <<= x
    m := 0
    x >>= x
    n := 0
    x >>>= x
    o := 0
    x &= x
    p := 0
    x |= x
    q := 0
    x ^= x
    r := 0

    s := 0
    // bit
    x = ~x
    t := 0
    x = x & x
    u := 0
    x = x | x
    v := 0
    x = x ^ x
    w := 0
    x = x << x
    y := 0
    x = x >> x
    z := 0
    x = x >>> x
    aa := 0
    // logic
    x = !x
    ab := 0
    x = x || x
    ac := 0
    x = x && x
    ad := 0
    // comparison
    x = x == x
    ae := 0
    x = x != x
    af := 0
    x = x < x
    ag := 0
    x = x > x
    ah := 0
    x = x <= x
    ai := 0
    x = x >= x
    aj := 0
    // unary
    x = x++ - 3
    ak := 0
    x = x-- + 3
    al := 0
}
