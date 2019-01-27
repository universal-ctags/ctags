proc ::pr0 {s} {
    puts "$s"
}

namespace eval N {
    proc pr1 {s} {
	puts "$s"
    }
}
