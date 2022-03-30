-- a = 1

{- b = 2 -}

{-
c = 3
-}

{-* d = 4 -}

{-*
e = 5
 -}

{- some comment -}
f = 6 {- another comment -}

  -- This should be ignored

-- This next line is not top level
{- so should continue the above -} + 1

-- A top level statement

h = 9

{-
comment
    {- nested comment -}
i = 9
-}
