fn main() {
	if a is T {
		a := 0
	} else if a !is T {
		b := 0
	} else if a in b {
		c := 0
	} else if a !in b {
		d := 0
	}
	$if a is T {
		a := 0
	} $else $if a !is T {
		b := 0
	} $else $if a in [T, U, V, $enum, $array, $float, $sumtype, $int, $struct, $function] {
		c := 0
	} $else $if a !in [T, U, V, $enum, $array, $float, $sumtype, $int, $struct, $function] {
		d := 0
	}
}
