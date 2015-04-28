package main

import "fmt"

type (
	T1 map[string]int
	T2 <-chan float32
	T3 chan []string
	T4 chan<- *[12]string
	T5 interface {
		Reader()
		Writer()
		foo()
	}
)

type T6 struct {
	_a, _b, _c, _d int
	int
	T1 `annotation`
	*T2
	_e float32
	//ignored int
}

const (A = iota;B;C;
	D = iota << (1 + iota*2)
	E
	F=3.14*(1+2*3)/34e7;I=1)

type (T7 func (a struct{_ int; _ float32}, b int) (int, map[string]int);T8 float32)

var (a, b, c int
d T5
e T4
f interface{})

func f1() {};func f2() {};type/*no newline here*/T9 int/*var ignored int
const ignored int*/const (G=6); var g int

func (t *T1) f3() (a, b int){
	return 1, 2
}; var h int

func (tt * T7) f4(a func () func ()) (func (), int) {return func (){}, 1};func f5(){};const H=1

func main() {
	go func (){}()
	fmt.Println("Hello, 世界")
}
