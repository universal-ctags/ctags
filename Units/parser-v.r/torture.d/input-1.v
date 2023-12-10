module thismodule

import that.aaa
import that.bbb { Bbb }

struct Foo {
	a aaa.Aaa
	b Bbb
}

fn main() {
	foo := Foo{}
	foo.a
	foo.b
}
