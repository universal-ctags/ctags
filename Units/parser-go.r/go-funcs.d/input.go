package main

import "net"

func f1() {
}

func f2(a int) {
}

func f3(a int) string {
	return ""
}

func f4(a, b, c <-chan int, d, e, f string) (A, B, C int, D string) {
	return 1, 2, 3, ""
}

type T int

func (t *T) f5(/* comment */ a, /*comment*/ /*comment*/       b string, 
	c  		[]int) (  /* */ int, string ) {
	return 1, ""
}

func (t *(T)) f6(r struct {a int `foo`; b string `bar`}) net.IP {
	return net.IP{}
}

func main() {
}
