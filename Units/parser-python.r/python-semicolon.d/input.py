
VAR1 = 1; VAR2 = 2;

class Cls:
    memb1 = 1; memb2 = 2

    def func(): pass;

    @staticmethod
    def func2(x): x += 1; y = x + 1; return y;


assert(VAR1 == 1)
assert(VAR2 == 2)
assert(Cls.memb1 == 1)
assert(Cls.memb2 == 2)
assert(Cls.func2(2) == 4)
