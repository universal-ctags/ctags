//
// Taken from http://doc.rust-lang.org/guide.html
//
fn main() {
	println!("Hello, world!");
}
struct Circle {
    x: f64,
    y: f64,
    radius: f64,
}

impl Circle {
    fn area(&self) -> f64 {
        std::f64::consts::PI * (self.radius * self.radius)
    }
}
