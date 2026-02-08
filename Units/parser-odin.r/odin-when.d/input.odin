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
