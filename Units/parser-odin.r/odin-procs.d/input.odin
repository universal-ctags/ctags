package procs

Comparator :: #type proc(x, y: int) -> int

init :: proc() {
}

vec2_add :: proc(x, y: f32) -> f32 {
    return x + y
}

div_check :: proc(a: f32) -> (int, bool) {
    return int(a), a > 0
}

write_bytes :: proc "c" (n: i32, data: rawptr) {
}

get_errno :: proc "cdecl" () -> i32 {
    return 0
}

clamp_value :: proc(x: $T) -> T where T == int {
    return x
}

add_ints :: proc(a, b: int) -> int {return a + b}
add_floats :: proc(a, b: f32) -> f32 {return a + b}
add :: proc {
    add_ints,
    add_floats,
}
