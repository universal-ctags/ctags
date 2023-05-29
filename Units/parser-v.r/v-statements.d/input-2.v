fn test_defer_return() {
    x := get_lock()
    defer {
        x.unlock()
    }
}

fn test_return_with_expr() {
    return x.value()
}

fn test_return_wo_expr() {
    return
}

fn test_assert() {
    assert a == a
}

fn test_is_operator() {
    if a is Cat {
        a.meow()
    }
}
