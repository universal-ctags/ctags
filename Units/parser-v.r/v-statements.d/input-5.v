fn main() {
	rlock m {
		a := 5
	}
	lock s, r {
		a := 5
	}
}
