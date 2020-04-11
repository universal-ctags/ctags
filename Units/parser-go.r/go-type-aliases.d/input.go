// Taken from https://go.googlesource.com/proposal/+/master/design/18130-type-alias.md
package main

type y int
type T2 int

type t y
type T1 = T2
type Name1 map[string]string
type Name2 map[string]string
type Alias = map[string]string

type X =
	map [
	string
] string

func main() {
	return;
}
