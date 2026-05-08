function Bar() {}

function Foo() {
	Bar.prototype.constructor.call(this)
	this.baz = null
}

Foo.prototype.foo = function() {}
