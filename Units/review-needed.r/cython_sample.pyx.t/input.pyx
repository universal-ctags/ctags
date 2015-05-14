#  -*- cython-mode -*-
# test code for python/cython functionality

python_var = 1

cdef int i = 2   # cython identifiers are not indexed
cdef :
    int j = 3
cdef k = 4 # no type here

cdef int int_identity(int i) : return i  

# here is a long one
cdef object int2string(int i) :
    return str(i)

# a cdef class
cdef class CDefClass :
    def __init__(self) :
        pass
    def standard_method(self,i) :
        return i
    cdef int c_method(self,int i) :
        return i

# a python function
def identity(x) :
    return x

# a python class
class StdClass :
    def return_me(self) :
        return "me"

cdef CDefClass cdefObj = CDefClass()
stdObj  = StdClass()

print "cython_sample: testing to make sure this file compiles and runs..."
print "i is: ", i
print "j is: ", j
print "i via identity: ", int_identity(i)
print "i via int2string: ", int2string(i)
print "cdefObj: ", cdefObj.c_method(i)
print "k via py identity", identity(k)
print "py method call:", stdObj.return_me()

