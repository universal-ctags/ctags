-- Binary operators and decimals

funcA a1 a2 =
    a1 + a2 - 3.3 * 3e-3 / .1

-- More binary operators

funcB b1 b2 =
    b1 |> b2 >> 3.3 /= 4 + 5

-- Hex numbers

funcC c1 =
    0xfa0 /= 0xFAE0

-- Parentheses

funcD d1 d2 =
    (3e4 + (5^7 - 0xe) // .2)

-- Call prefix functions

funcE e1 =
    e1 + myFunc 3 4 + 5

-- Multiline strings

funcF f1 =
    f1 ++ """This is a multiline
string, which ends after
this line
"""

-- Strings

funcG g1 =
    "Double q\"uote, \n, etc" ++
    "uni\u{04FA2}code"

-- Characters

funcH h1 =
    'D' ++ '\"' ++ '\'' ++ '\n' ++
    '\u{04FA2}'

-- Anonymous functions

funcI i1 =
    (\ x y z -> x+y - z)

