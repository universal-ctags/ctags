-- Expressions with parentheses

funcA =
    (23 + 34)

-- Anonymous functions

funcB = (\b1 b2 b3 -> 42)

-- Allow us to call constructors as functions

funcC =
    Cons a b

-- Allow us to functions and constructors using their module names

funcD =
    (List.Deep.Cons a b) + (Main.add a b)

-- Basic lists

funcE e1 e2 =
    e1 :: [e2, e1 + e2]

-- Empty lists

funcF = []

-- Tuples, including an empty tuple

funcG =
    (12, 0x034, ('a')) ++ ()

-- Record names as function calls

funcH h =
    (3) + (.age h.family.person)

-- Binary operators as function calls

funcI i1 i2 =
    (++) (i1 // i2) 99

-- Records

funcJ1 =
    {x = 2 , y = 3 , z = 4}

funcJ2 =
    { point | x = point.x + 1 , y = Cons a b }

funcJ3 =
    {}

-- Record with spaces

funcJ4 =
    { model
    | width = width
    }
