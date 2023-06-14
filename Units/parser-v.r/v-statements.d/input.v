fn test_if() {
    a := 0 // test
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
    } // // test
    else if b {
        f := 0
    } // test
    else {
        g := 0
    }
    if a {
        if b {
            h := 0 /* test */
        }
        else {
            i := 0 /* test /* NESTED */ test */
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
    if mut x is SomeStruct {
        l := x
    }
}

/*
MULTILINE
/*
NESTED
*/
COMMENT
*/
