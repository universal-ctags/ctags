-- A function with an if/then/else statement

-- The code below won't compile, but it is
-- syntactically correct

funcA a1 a2 =
    if (complex ex pression + 12) == "Literal"
    then
        a + b
    else
        a +
        b

-- Check we can still read a simple function?

funcB b1 =
    True

-- Can we catch functions defined inside if statements?

funcC c1 c2 =
    if (complex ex pression + 12) == "Literal"
    then
        let
            c2 = 2
            c3 = 3
        in
        c2 + c3
    else
        a +
        b
