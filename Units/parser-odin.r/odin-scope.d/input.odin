package scope

Point :: struct {
    x: f32,
    y: f32,
}

Color :: enum {
    Red,
    Green,
    Blue,
}

origin :: proc() -> Point {
    return Point{0, 0}
}
