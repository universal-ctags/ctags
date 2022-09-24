oo::class create S {
    variable x
    method sx {} {
        set x 0
    }
}
oo::class create M {
    variable x
    method mx {} {
        incr x 3
    }
}
oo::class create C {
    superclass S
    mixin M
    variable x
    method cx {} {
        incr x 5
    }
}
# Taken from https://wiki.tcl-lang.org/page/Variables+in+TclOO
