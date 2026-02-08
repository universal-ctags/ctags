package ffi

foreign import libc "system:libc.so"

foreign libc {
    puts :: proc "c" (s: cstring) -> i32 ---
    exit :: proc "c" (code: i32) ---
    errno: i32
    signal_handler: proc "c" (sig: i32)
}

foreign import mruby "libmruby.a"

@(default_calling_convention = "c")
foreign mruby {
    @(link_name = "mrb_open")
    open :: proc() -> rawptr ---
    close :: proc(state: rawptr) ---
}
