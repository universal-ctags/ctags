-- We should be able to parse this function

funcA a1 a2 = a1 + a2

-- We should get only funcB, not funcC

funcB b =
    b + """This is a multiline
string, which ends after

funcC = 3
"""

funcD d1 =
    (d1 + 34)

-- The end
