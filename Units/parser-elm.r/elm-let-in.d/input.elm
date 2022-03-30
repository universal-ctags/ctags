-- Basic let/in block

funcA =
    let
        a1 = 1
        a2 x y = x + y
    in
    a1 << a2

-- Nested let/in blocks

funcB b =
    let
        b1 =
            let
                b2 x y =
                let
                    b3 x y =
                        x * y
                    b4 v w =
                        v * w
                in
                b3 b2 99
            in
            (flip << b2) 77
        b5 i j = i + j
    in
    b1 b

-- Let/in block with type annotation

funcC =
    let
        c1 : Int
        c1 = 1

        c2 : Int Float -> Something
        c2 x y = x + y
    in
    c1 << c2
