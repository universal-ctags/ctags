package main
import (
	"fmt"
)

type Point struct { X, Y int }

func (p *Point) Render() {
	fmt.Printf("<%d,%d>\n", p.X, p.Y)
}

func main() {
	p := &Point{X: 5, Y: 12}
	p.Render()
}

func (*Point) doNothing0() {
}

func (Point) doNothing1() {
}

func () wrongSyntax0() {
}

func (+) wrongSyntax1() {
}

func (a-) wrongSyntax2() {
}

func (brokenInput
