#! fn ignored_in_comment() {}
#![feature(globs)]
#![feature(macro_rules)]
use std::*;
mod test_input2
{
	pub struct SomeStruct;
}

fn lifetime_and_char<'lifetime>(_: &'lifetime isize)
{
	let s = '"';
	let s = '}';
	let s = '\'';
	fn not_hidden_by_char() {}
}

fn preserve_string_delims(_bar: extern r#"C"# fn()) {}

pub struct A
{
	foo: fn() -> isize,
	bar: isize
}

pub struct B
{
	#[cfg(test)]
	foo: isize,
	bar: isize
}

pub struct C<T> where T: Send
{
	a: T
}

pub trait D<T> where T: Send
{
}

impl<T> D<T> for C<T> where T: Send
{
}

pub fn where_foo<T>(a: T) where T: Send
{
}

fn array_param(arr: [[u32; 3]; 4])
{
}

/*
 * fn ignored_in_comment() {}
 */

// fn ignored_in_comment() {}

/* /*
 * */
 fn ignored_in_nested_comment() {}
 */

static size: usize = 1;
const N: usize = 10;

#[cfg(test)]
struct S1 {
	only_field: [isize; size]
}

macro_rules! test_macro
{
	() => {1}
}

macro_rules! ignore {($($x:tt)*) => (())}

fn yada(a:isize, c:Foo, b:test_input2::SomeStruct) -> String {
	a.to_string()
}

fn main() {	
	use test_input2::*;
	let a=Foo{foo_field_1:2};
	a.my_method(1);
	let c=Animal::a_cat(3);
	let d=Foo{foo_field_1:a.foo_field_1+2}; a.test();
	println!("{}", a.foo_field_1.to_string());
	ignore!
	(
		fn ignored_inside_macro() {}
	);
	ignore!
	[
		fn ignored_inside_macro() {}
	];
	ignore!
	{
		fn ignored_inside_macro() {}
	}

	let _ = "fn ignored_in_string() {}
	";

	let _ = r##"fn ignored_in_raw_string() {}""##;

	fn nested() {}
}

struct Bar(isize);

struct Baz(isize);

struct Foo{foo_field_1:isize}

struct Foo2 {
		x:isize,
		y:isize
}

impl Foo {
	fn my_method(&self,_:isize){ println!("{}", "my_method of foo");}
}

enum Animal {
	a_anteater(isize),
	a_bear(isize),
	a_cat(isize),
	a_dog(isize),
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
		println!("{}", self.foo_field_1.to_string());
	}

	fn test1(&self) {
		println!("{}", self.foo_field_1.to_string());
	}

	fn test2(&self) {
		println!("{}", self.foo_field_1.to_string());
	}
}

impl DoZ for Foo {
	fn do_z(&self) {
		println!("{}", self.foo_field_1.to_string());
	}
}

trait SuperTraitTest:Testable+DoZ {
}

fn gfunc<X:Testable+DoZ>(x:&X) {
	let a1=Animal::a_anteater(1);
	let a2=Animal::a_bear(1);
	let a3=Animal::a_cat(1);
	let a4=Animal::a_dog(1);
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
		Animal::a_cat(x)=> println!("{}", "cat"),
		_ => println!("{}", "not a cat")
	}
}
