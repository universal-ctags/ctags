
VARa1, LAMBDAa1 = 1, lambda x: x*x
LAMBDAa2, VARa2 = lambda x: x*x, 2
VARa3, LAMBDAa3, VARa4 = 3, lambda x: x*x, 4

VARb1, LAMBDAb1 = (1,1), lambda x: (x, x)
LAMBDAb2, VARb2 = lambda x: (x, x), (2,2)
VARb3, LAMBDAb3, VARb4 = (3,3), lambda x: (x, x), (4,4)

VARc1, LAMBDAc1 = (1, 1), lambda x,y: x+y
LAMBDAc2, VARc2 = lambda x,y: x+y, (2,2)
VARc3, LAMBDAc3, VARc4 = (3,3), lambda x,y: x+y, (4,4)

VARd1, VARd2 = (lambda x: (x, x+1))(1)
VARd3, VARd4 = (3, 4)

# check Python actually likes it
assert(VARa1 == 1)
assert(LAMBDAa1(1) == 1)
assert(VARa2 == 2)
assert(LAMBDAa2(2) == 4)
assert(VARa3 == 3)
assert(LAMBDAa3(3) == 9)
assert(VARa4 == 4)

assert(VARb1 == (1,1))
assert(LAMBDAb1(1) == (1,1))
assert(VARb2 == (2,2))
assert(LAMBDAb2(2) == (2,2))
assert(VARb3 == (3,3))
assert(LAMBDAb3(3) == (3,3))
assert(VARb4 == (4,4))

assert(VARc1 == (1,1))
assert(LAMBDAc1(1,1) == 2)
assert(VARc2 == (2,2))
assert(LAMBDAc2(2,2) == 4)
assert(VARc3 == (3,3))
assert(LAMBDAc3(3,3) == 6)
assert(VARc4 == (4,4))

assert(VARd1 == 1)
assert(VARd2 == 2)
assert(VARd3 == 3)
assert(VARd4 == 4)
