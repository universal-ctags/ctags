fn test_if() {
    a := 0
    if a {
        b := 0
    }
    if a {
        c := 0
    }
    else {
        d := 0
    }
    if a {
        e := 0
    }
    else if b {
        f := 0
    }
    else {
        g := 0
    }
    if a {
        if b {
            h := 0
        }
        else {
            i := 0
        }
    }
    else {
        if c {
            j := 0
        }
        else {
            k := 0
        }
    }
}
