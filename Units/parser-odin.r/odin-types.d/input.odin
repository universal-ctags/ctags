package types

Meter :: distinct f64
Handle :: distinct rawptr

Ref :: u8
Lookup :: map[string]int

Days :: enum {
    Mon,
    Tue,
    Wed,
    Thu,
    Fri,
    Sat,
    Sun,
}

Day_Set :: bit_set[Days]

Flags :: bit_field u16 {
    x: i32  | 3,
    y: u16  | 5,
    z: bool | 1,
}

Vec3 :: [3]f32
Ptr_Int :: ^int
