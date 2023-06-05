fn main() {
    x := foo()
    defer {
        x.unfoo()
    }
}
