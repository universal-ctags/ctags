package attrs

@(private)
internal_state := 10

@(private = "file")
restore_mode :: proc() {}

@(deprecated = "use destroy", private = "file")
cleanup :: proc() {}

destroy :: proc() {}
