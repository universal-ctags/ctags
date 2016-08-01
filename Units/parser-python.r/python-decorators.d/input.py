
def noop(f):
    return f

def noopwrapper(*args):
    return noop

@noop
def func01():
    pass

@noop
def func02(): pass
def func03(): pass
@noop
def func04(): pass


@noop
class class01(object):
    @staticmethod
    def hello():
        print("hello")

    @classmethod
    def hi(cls):
        print("hi from %s" % cls.__name__)

    @noopwrapper(1, 2, 3)

    @   staticmethod
    @noop
    def dummy():
        print("I'm not useful")

    @staticmethod
    def tracer(f):
        def wrapper():
            print("tracing %s" % f.__name__)
            return f()
        return wrapper


@class01.tracer
def func05():
    class01.hello()

@ class01 .	tracer
def func06():
    class01.hi()
