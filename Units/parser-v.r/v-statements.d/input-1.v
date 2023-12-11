fn test_for() {
    // in
    for a in x {
    }
foo:
    for mut i, b in x {
    }
    for mut c in x {
    }
    for i, mut d in x {
    }
bar:
    for _, e in x {
    }
    for f, _ in x {
        break
    }
    // range
    for g in 0..5 {
        break foo
    }
    // cond
    for i <= 100 {
        continue
    }
    // bare
    for {
        continue bar
    }
    for i := 0; i < 100; i++ {
    }
    // other expr
    for (i & whatever) == true {
    }
    for if a { b } else { c } == 8 {
    }
    for p, q := a.b.c, x.y.z; p < q; {
    }
}
