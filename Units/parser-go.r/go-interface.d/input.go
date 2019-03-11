//
// Taken from https://golang.org/ref/spec#Interface_types
//
// GOPATH=$SOMEWHERE/ctags/Units/parser-go.r/go-interface.d go run $SOMEWHERE/ctags/Units/parser-go.r/go-interface.d/input.go
//
package main

import "ext"

type Buffer interface {
}

type Locker interface {
	Lock()
	Unlock()
}

type ReadWriter interface {
	Read(b Buffer) bool
	Write(b Buffer) bool
}

type File interface {
	ReadWriter  // same as adding the methods of ReadWriter
	Locker      // same as adding the methods of Locker
	ext.Processor
	Close()
	ListAttr() ([][]byte, error)
}

func main () {
}
