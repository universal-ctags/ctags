// not even consts!
fn test_exprs() {
    a := x + x + x
    b := x - x - x
    c := x * x * x
    d := x / x / x
    e := x % x % x
    f := x ^ x
    // assignment
    g := x += x
    h := x -= x
    i := x /= x
    j := x *= x
    k := x %= x
    l := x <<= x
    m := x >>= x
    n := x >>>= x
    o := x &= x
    p := x |= x
    q := x ^= x
    r := 0
    // bit
    s := ~x
    t := x & x
    u := x | x
    v := x ^ x
    w := x << x
    y := x >> x
    z := x >>> x
    // logic
    aa := !x
    ab := x || x
    ac := x && x
    // comparison
    ad := x == x
    ae := x != x
    af := x < x
    ag := x > x
    ah := x <= x
    ai := x >= x
    // unary
    aj := x++ - 3
    ak := x-- + 3
}
