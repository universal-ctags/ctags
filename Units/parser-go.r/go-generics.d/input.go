package x

// Taken from https://go.dev/ref/spec#Function_types
func min[T ~int|~float64](x, y T) T {
	if x < y {
		return x
	}
	return y
}


// Taken from podman/vendor/golang.org/x/exp/slices/slices.go
func EqualFunc[S1 ~[]E1, S2 ~[]E2, E1, E2 any](s1 S1, s2 S2, eq func(E1, E2) bool) bool {
	if len(s1) != len(s2) {
		return false
	}
	for i, v1 := range s1 {
		v2 := s2[i]
		if !eq(v1, v2) {
			return false
		}
	}
	return true
}

func F0[L ~[]M, M comparable, _, G any](s L, v M) int {
    return -1
}

func F1[N ~[]O, O comparable, _ any](s N, v O) int {
    return -1
}
