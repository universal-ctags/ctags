
def func1():
    pass

def func2(arg1):
    pass

def func3(arg1, arg2):
    pass

def func4( arg1 ,
           arg2 ,
           arg3 ):
    pass

def func5( arg1 = 1,
           arg2 = r'hello',
           arg3 = None):
    pass

def func6(arg1, arg2, *args):
    return (arg1 + 1, arg2 + 2) + args

def func7(*args, **kwargs):
    pass

def func8(arg1=1*2, arg2=func6(1, 2, 3, 4)):
    pass

VAR1=0

def func9(arg1=VAR1*VAR1):
    pass

def func10(arg1=[VAR1,VAR1],
           arg2={VAR1:VAR1}):
    pass


class Cls(object):
    def __init__(self):
        pass

    def method1(self, arg1):
        pass

    def method2(self, arg1 = 0, arg2=None):
        pass

    @staticmethod
    def method3(arg1, arg2=0):
        pass
