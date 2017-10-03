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
