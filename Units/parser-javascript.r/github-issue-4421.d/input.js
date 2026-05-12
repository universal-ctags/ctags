function Bar() {}

function Foo() {
	Bar.prototype.constructor.call(this)
	this.baz = null
}

Foo.prototype.foo = function() {}

function Baz() {
	Bar.prototype.constructor = call(this)
	this.baz = null
}

Baz.prototype.baz = function() {}