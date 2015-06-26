# bug #1055
import unittest
type TMatrix*[N,M: static[int], T] = object
  data*: array[0..N*M-1, T]
proc `==`*(a: distinct TMatrix; b: distinct TMatrix): bool =
  result = a.data == b.data

proc p(x, y: int): int =
  result = x + y

echo p((proc (): int =
          var x = 7
          return x)(),
       (proc (): int = return 4)())

proc `1/1`() = echo(1 div 1)
template `1/2`() = echo(1 div 2)
var `1/3` = 1 div 4
`1/3` = 1 div 3 # oops, 1/3!=1/4
let `1/4` = 1 div 4

`1/1`()
`1/2`()
echo `1/3`
echo `1/4`
