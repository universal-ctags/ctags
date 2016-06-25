#![cfg(not(test))] fn not_hashbang() {}

pub fn foo_bar_test_func(apples:fruit::SomeStruct,(oranges,lemon):(isize,isize))->isize{
	let some_var_name=2*oranges;
	let a=SomeLongStructName{v:0};
	println!("{}", "a");
	veg::another_function(apples.red_value,oranges,lemon);
	some_var_name-apples.red_value+lemon+a.v
}

pub mod fruit {
	pub struct SomeStruct{
		pub red_value: isize,
		pub green_value: isize,
		pub blue_value: isize
	}
}

fn free_func() {
}

impl SomeLongStructName {
	fn fooo() {
	}
	fn baaz() {
	}
}

pub struct SomeLongStructName {v:isize}

mod veg{
	pub fn another_function(a:isize,b:isize,c:isize)->isize {
		a+b+c
	}
}

mod mineral {
	fn granite() {
	}
	fn limestone() {
	}
	fn chalk() {
	}
}

fn main() {}
