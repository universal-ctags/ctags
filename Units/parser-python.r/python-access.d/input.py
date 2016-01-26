
_CONST1 = 42
__CONST2 = 43

class _NotAutoImported:
    pass

class __JustTheSame:
    pass

def _and_again():
    def sub_is_private():
        pass
    def _even_with_underscore():
        pass
    class ClassesToo:
        pass
    class _ForReal:
        pass
    pass

def __still_the_same():
    pass

def but_thankfully_im_pulic():
    pass

class C:
    def __init__(self):
        pass

    def __repr__(self):
        return 'C'

    def _foo(self):
        pass

    def __bar(self):
        pass

    def baz(self):
        pass

    class __Cpriv:
        pass
