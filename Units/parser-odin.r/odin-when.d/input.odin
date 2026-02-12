package when_test

when ODIN_OS == .Windows {
    PATH_SEP :: "\\"
    platform_init :: proc() {}
} else when ODIN_OS == .Linux {
    PATH_SEP :: "/"
    platform_init :: proc() {}
} else {
    PATH_SEP :: "/"
    platform_init :: proc() {}
}

when size_of(rawptr) == 8 {
    ARCH_64 :: true
}

when #config(ENABLE_EXTRAS, false) {
    WORD_SIZE :: 8
}
