package main

func f0(nums...int) {
	return
}

func f1(nums  ...	interface{}) {
	return
}

func f10(nums...	interface{}) {
	return
}

func f11(nums  ...interface{}) {
	return
}

func f2(o int, nums ...int) {
	return
}

func f3(o int, nums...  /* ... */		int) {
	return
}

func f4(o              int,          p   int) {
}

func main() {
}
