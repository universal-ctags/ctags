# https://www.python.org/dev/peps/pep-0465/

import numpy as np
from numpy.linalg import inv, solve

@something
def dotFunc():
    # Using dot function:
    S = np.dot((np.dot(H, beta) - r).T,
               np.dot(inv(np.dot(np.dot(H, V), H.T)), np.dot(H, beta) - r))

@something
def dotMethod():
    # Using dot method:
    S = (H.dot(beta) - r).T.dot(inv(H.dot(V).dot(H.T))).dot(H.dot(beta) - r)

@something
def atOpv1():
    # Version 1
    S = (H @ beta - r).T @ inv(H @ V @ H.T) @ (H @ beta - r)

def atOpv2():
    # Version 2
    trans_coef = H @ beta - r
    S = trans_coef.T @ inv(H @ V @ H.T) @ trans_coef

@something
def atOpv3():
    # Version 3
    S = trans_coef.T @ solve(H @ V @ H.T, trans_coef)
