#  -*- cython-mode -*-
# test code for cython functionality with complex datatypes

import numpy as np
cimport numpy as np

cpdef np.ndarray[dtype=double, ndim=1] my_fun(np.ndarray[dtype=double, ndim=1] x):
    cdef np.ndarray[dtype=double, ndim=1, mode="c"] res

    res = 2*x
    return res
