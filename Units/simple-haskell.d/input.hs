module Cards where

data Int = -2147483648 | -2147483647 | ... | -1 | 0 | 1 | 2 | ... | 2147483647
newtype Fd = Fd CInt
data Bool = True | False

add :: Integer -> Integer -> Integer
add x y  = x + y
