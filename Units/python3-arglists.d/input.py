#!/usr/bin/env python3

def func01():
    pass
def func02(a):
    pass
def func03(a, b = 2):
    pass
def func04(a = 1, b = 2):
    pass
def func05(*args):
    pass
def func06(**kwargs):
    pass
def func07(*args, **kwargs):
    pass
def func08(a, b = 2, *args):
    pass
def func09(a = [1, 2], b = 2, **kwargs):
    pass
def func10(a = (1, 2), b = 2, *args, **kwargs):
    pass
def func11(a = {1:1, 2:1}, b = 2, *args, **kwargs):
    pass

# Python3 only
def func12(a:"first", b:"second"):
    pass
def func13(a:"first", b):
    pass
def func14(a:"first" = 1, b = 2):
    pass
def func15(a:"first" = (1, 2), *args:'more args'):
    pass
# crazier, but supported (as what follows ":" is an expression)
def func16(a:1+1 = (1, 2), *args:2*2):
    pass
def func17(a:{"1":1, "2":2} = {"1":1, "2":2}, *args:lambda:0):
    pass
def func18(a:lambda:[1,2][:1] = (2+2)*2, *args:lambda:0):
    pass

lamb01 = lambda: 0
lamb02 = lambda a: 0
lamb03 = lambda a, b: 0
lamb04 = lambda a, b = 2: 0
lamb05 = lambda *args: 0
lamb06 = lambda *args, **kwargs: 0
lamb07 = lambda a, *args, **kwargs: 0
lamb08 = lambda a = 1, *args, **kwargs: 0
lamb09 = lambda a = [1, 2], *args, **kwargs: 0
lamb10 = lambda a = (1, 2), *args, **kwargs: 0
lamb11 = lambda a = {1:1, 2:1}, *args, **kwargs: 0
lamb12 = lambda a = lambda:0, *args, **kwargs: a
