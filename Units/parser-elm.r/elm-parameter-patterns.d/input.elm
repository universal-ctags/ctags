-- Patterns can be found at https://elmprogramming.com/pattern-matching.html

-- Simple tuple

funcA (a1, _, a2) =
    a1 + a2

-- Multiple tuples

funcB (b1, b2) (b3, b4) =
    b1 + b2

-- Records with named fields

funcC {c1} {c2, c3} =
   c1 + c2 + c3

-- Constructor patterns
 
funcD (D1Cons a b) (D2Cons a b, D3Cons a b) =
    b + 1

-- Combining the above
 
funcE (D1Cons {a, b} c) d =
    c + 1

-- Using 'as' clauses
 
funcF (D1Cons ({a, b} as ab) c) (d as d2) =
    c + 1

-- Make sure complex parameters can be used in anonymous functions

funcG =
    (\ (D1Cons {a, b} c) d -> a + b + c + d)
