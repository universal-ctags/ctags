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

foreign import gl { "system:opengl32.lib", "system:glu32.lib" }

foreign libc {
    @private
    internal_init :: proc "c" () ---
    readv :: proc "c" (fd: i32, iov: rawptr, cnt: i32) -> i32 ---
    getres :: proc "c" (w: ^i32, h: ^i32) -> (i32, i32) ---
    printf :: proc "c" (fmt: cstring, #c_vararg args: ..any) -> i32 ---
    get_buf :: proc "c" (n: i32) -> [4]i32 ---
    when ODIN_OS == .Linux {
        epoll_create :: proc "c" (size: i32) -> i32 ---
    }
}
