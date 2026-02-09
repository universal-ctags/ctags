package procs

import "core:strings"

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

My_Callback :: proc(x: int) -> bool
Loader :: proc(path: string) -> ^u8
alloc_buf :: proc(size: int, allocator := context.allocator) -> []u8 {
    return nil
}
get_items :: proc() -> [dynamic]int {
    return nil
}
make_handler :: proc() -> proc(int) {
    return nil
}
get_builder :: proc() -> strings.Builder {
    return {}
}

get_lookup :: proc() -> map[string]int {
    return nil
}

get_c_callback :: proc() -> proc "c" (i32) {
    return nil
}

get_transform :: proc() -> proc(int) -> int {
    return nil
}

get_checker :: proc() -> proc(int) -> (bool, int) {
    return nil
}

get_simd :: proc() -> #simd[4]f32 {
    return {}
}

Number :: struct($T: typeid) { val: T }
get_number :: proc() -> Number(int) {
    return {}
}

foo_no_alias :: proc(#no_alias a, b: ^int) {}

print_caller_location :: proc(loc := #caller_location) {}

optional_result :: proc(x: int) -> (value: int, ok: bool) #optional_ok {
    return x, true
}

proc_no_type_assert :: proc(a: any) -> int #no_type_assert {
    return 0
}

apply :: proc(f: proc(int) -> int, x: int) -> int {
    return f(x)
}

add_inline :: #force_inline proc(a, b: int) -> int {
    return a + b
}

spaced :: proc(a: int ) -> int {
    return a
}
