#! fn ignored_in_comment() {}
#[feature(globs)];
#[feature(macro_rules)];
use std::*;
use std::io::stdio::println;
use test_input2::*;
mod test_input2;

pub struct A
{
	foo: fn() -> int,
	bar: int
}

pub struct B
{
	#![cfg(test)]
	#[cfg(test)]
	foo: int,
	#![cfg(test)]
	bar: int
}

/*
 * fn ignored_in_comment() {}
 */

// fn ignored_in_comment() {}

/* /*
 * */
 fn ignored_in_nested_comment() {}
 */

static size: uint = 1;

struct S1 {
	only_field: [int, ..size]
}

macro_rules! test_macro
{
	() => {1}
}

macro_rules! ignore (($($x:tt)*) => (()))

fn yada(a:int,c:Foo,b:test_input2::fruit::SomeStruct) -> ~str {
	a.to_str()
}

fn main() {	
	use test_input2::fruit::*;	
	io::println(foo_bar_test_func(SomeStruct{red_value:1,green_value:2,blue_value:3},(4,5)).to_str());
	let a=Foo{foo_field_1:2};
	a.my_method(1);
	let c=a_cat(3);
	let d=Foo{foo_field_1:a.foo_field_1+2}; a.test();
	println(a.foo_field_1.to_str());
	ignore!
	(
		fn ignored_inside_macro() {}
	)
	ignore!
	[
		fn ignored_inside_macro() {}
	]

	let _ = "fn ignored_in_string() {}
	";

	let _ = r##"fn ignored_in_raw_string() {}""##;

	fn nested() {}
}

struct Bar(int);

struct Baz(int);

struct Foo{foo_field_1:int}

struct Foo2 {
		x:int,
		y:int
}

impl Foo {
	fn my_method(&self,_:int){ println("my_method of foo");}
}

enum Animal {
	a_anteater(int),
	a_bear(int),
	a_cat(int),
	a_dog(int),
}

trait Testable 
{	fn test(&self);
	fn test1(&self);
	fn test2(&self);
}

trait DoZ {
	fn do_z(&self);
}

impl Testable for Foo {
	fn test(&self) {
		println(self.foo_field_1.to_str());
	}

	fn test1(&self) {
		println(self.foo_field_1.to_str());
	}

	fn test2(&self) {
		println(self.foo_field_1.to_str());
	}
}

impl DoZ for Foo {
	fn do_z(&self) {
		println(self.foo_field_1.to_str());
	}
}

trait SuperTraitTest:Testable+DoZ {
}

fn gfunc<X:Testable+DoZ>(x:&X) {
	let a1=a_anteater(1);
	let a2=a_bear(1);
	let a3=a_cat(1);
	let a4=a_dog(1);
	x.test();
	x.do_z();
}

struct TraitedStructTest<X> {
	x:X
}

trait ParametrizedTrait<T> {
	fn test(&self);
}

impl<T: Clone> ParametrizedTrait<T> for TraitedStructTest<T> {
	fn test(&self) {
	}
}

fn some2(a:Animal) {
	match a {
		a_cat(x)=> println("cat"),
		_ => println("not a cat")
	}

}
