package main

Vec2 :: struct {
    x: f32,
    y: f32,
}

Direction :: enum {
    North,
    East,
    South,
    West,
}

add :: proc(a, b: Vec2) -> Vec2 {
    return Vec2{a.x + b.x, a.y + b.y}
}

main :: proc() {
    v := Vec2{1, 2}
}
