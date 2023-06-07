fn test_defer_return() {
    x := get_lock()
    defer {
        x.unlock()
    }
    return x.value()
    return
    assert a == a
}
