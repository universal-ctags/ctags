proc fn a {
    return a
}

proc f11 {a} {
    return a
}

proc f10 {{a 0}} {
    return a
}

proc g1121 {a b} {
    return a + b
}

proc g1021 {{a 0} b} {
    return a + b
}

proc g1020 {{a 0} {b 1}} {
    return a + b
}

proc g1120 {a {b 1}} {
    return a + b
}

proc h112131 {a b c} {
    return a + b + c
}

proc h102131 {{a 0} b c} {
    return a + b + c
}

proc h112031 {a {b 0} c} {
    return a + b + c
}

proc h112130 {a b {c 0}} {
    return a + b + c
}

proc h102031 {{a 0} {b 0} c} {
    return a + b + c
}

proc h112030 {a {b 0} {c 0}} {
    return a + b + c
}

proc h102130 {{a 0} b {c 0}} {
    return a + b + c
}

proc h102030 {{a 0} {b 0} {c 0}} {
    return a + b + c
}

set d 1

proc i10_0 {{a d}} {
    return a
}

proc i10_1 {{a {d}}} {
    return 0
}

proc i10_2 {{a {[h102030]}}} {
    return 0
}
