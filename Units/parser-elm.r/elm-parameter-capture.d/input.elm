-- Simple paramaeters

funcA a1 a2 =
    a1 + a2

-- With extra whitespace and type annotation

funcB : Int -> Int -> Int
funcB b1
    b2
    =
    b1 + b2

-- Complex parameters

funcC (c1, c2) {c3} (C4Cons c4 c5) =
    c1 + c2

-- No parameters

funcD = 4

-- No whitespace

funcE=5

-- Functions inside let/in block

funcF1 =
    let
        funcF2 f1 f2 = 6
    in
    6
