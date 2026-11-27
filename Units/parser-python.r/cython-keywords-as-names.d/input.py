# cdef, cimport, cpdef, extern and inline are keywords in Cython only
inline = True
extern = {}
cimport = 1
cdef = 2
cpdef = 3

from pkg import inline, other
from pkg import thing as extern
import mod as cdef


class Options:
    inline = False
    extern: bool = True

    def cdef(self, source):
        pass

    def cpdef (self):
        pass

    def inline(self):
        pass


def extern(name):
    pass


def cimport():
    pass


class inline:
    pass


def last(inline, extern=None):
    return inline
