-- A function with a case statement

-- The code below won't compile, but it is
-- syntactically correct

funcA a1 a2 =
    case (complex ex pression + 12) of
        "Literal" ->
            a + b
        'l' ->
            Cons a b
        Cons a b ->
            a +
            b
        _ ->
            False

-- Check we can still read a simple function

funcB b1 =
    True

-- Can we catch functions defined inside case statements?

funcC c1 c2 =
    case (complex ex pression + 12) of
        "Literal" ->
            let
                c2 = 2
                c3 = 3
            in
            c2 + c3
        _ -> False

-- This previously exposed a bug...

funcD str =
    case str of
      Just c1 ->
        c1 / 255
      _ ->
        0

