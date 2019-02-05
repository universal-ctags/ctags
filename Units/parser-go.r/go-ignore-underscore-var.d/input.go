package main

func F() (int, int) {
	return 1, 2
}

var x0, y0 = F()
var x, _ = F()
func main() {
	print (x)
	return
}
