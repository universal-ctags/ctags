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

Id_Map :: distinct map[string]int
Tile_Row :: distinct [16]u8
Ptr_Handle :: distinct ^u8
Key :: enum {
    Bronze =  1,
    Silver =  5,
    Gold   = 10,
}
Key_Descriptions :: #sparse[Key]string {
    .Bronze = "a blocky bronze key",
    .Silver = "a shiny silver key",
    .Gold   = "a glittering gold key",
}

INIT_VALUE :: (100 + 200)
