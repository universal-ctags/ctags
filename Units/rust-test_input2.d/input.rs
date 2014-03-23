use std::io::stdio::println;

pub fn foo_bar_test_func(apples:fruit::SomeStruct,(oranges,lemon):(int,int))->int{
	let some_var_name=2*oranges;
	let a=SomeLongStructName{v:0};
	println("a");println("b");	println("c");
	veg::another_function(apples.red_value,oranges,lemon);
	some_var_name-apples.red_value+lemon+a.v
}

pub mod fruit {
	pub struct SomeStruct{
		red_value:int,green_value:int,blue_value:int
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

pub struct SomeLongStructName {v:int}

mod veg{
	pub fn another_function(a:int,b:int,c:int)->int {
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
