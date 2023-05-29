fn main() {
    return match mut x {
        StructA { 0 }
        StructB { 0 }
        int, u16 { 0 }
        &a.b.c.Foo { 0 }
        ?map[string]f32 { 0 }
        []string { 0 }
        &a.b.c.Foo, ?map[string]f32, []string { 0 }
        else { 0 }
    }
}
