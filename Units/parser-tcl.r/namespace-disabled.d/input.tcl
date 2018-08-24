namespace eval A::B {
}

namespace eval A {
    proc pr1 {s} {
	puts $s
    }
    pr1 "a"
    proc B::pr2 {s} {
	puts "$s"
    }
    namespace eval C {
	proc pr3 {s} {
	    puts $s
	}
    }
    proc ::pr4 {s} {
	puts "$s"
    }

}

proc ::pr5 {s} {
    puts "$s"
}

proc pr6 {s} {
    puts "$s"
}

A::pr1 "b"

A::B::pr2 "c"

A::C::pr3 "d"

pr4 "e"

pr5 "f"

pr6 "g"
