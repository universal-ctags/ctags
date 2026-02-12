package structs

Point :: struct {
    x, y: f32,
}

Color :: struct #packed {
    r, g, b, a: u8,
}

Entity :: struct {
    using pos: ^Point,
    name:      string,
}

Generic_Pair :: struct($T: typeid) {
    first:  T,
    second: T,
}

Aligned :: struct #align(8) {
    val: i32,
    using data: [4]u8,
}

Engine :: struct {
    title:      string,
    resolution: [2]f32,
    debug:      bool,
    fonts:      struct {
        small:  rawptr,
        medium: rawptr,
    },
    metrics:    struct {
        fps_current: f32,
        frame_count: u32,
    },
    _:          struct {
        reserved: [8]u8,
    },
}
