# Reported by @m-novikov in #763
def foo():
    var = some1(some2())

def bar():
    pass

def baz(arg1, arg2=({()})):
    pass

gvar = test(())


def tagme():
    pass

