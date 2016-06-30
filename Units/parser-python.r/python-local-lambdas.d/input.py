globallambda01 = lambda x:x

def func01():
    locallambda01 = lambda x:x
    localvar01 = 1  # ignored

class class01:
    memberlambda01 = lambda x:x
    def member01(self):
        locallambda02 = lambda x:x
        localvar02 = 2
