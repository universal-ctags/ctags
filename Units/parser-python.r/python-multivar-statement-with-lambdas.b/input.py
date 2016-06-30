
VAR1, LAMBDA1 = 1, lambda x: x*x
LAMBDA2, VAR2 = lambda x: x*x, 2
VAR3, LAMBDA3, VAR4 = 3, lambda x: x*x, 4

# check Python actually likes it
assert(VAR1 == 1)
assert(LAMBDA1(1) == 1)
assert(VAR2 == 2)
assert(LAMBDA2(2) == 4)
assert(VAR3 == 3)
assert(LAMBDA3(3) == 9)
assert(VAR4 == 4)
