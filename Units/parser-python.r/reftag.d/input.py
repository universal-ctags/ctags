import a.b

class X(object):
    def __init__(self, n):
        self.n = n

    def val(self):
        return self.n

def one():
    return X(1)

def two():
    return X(2)

print (one().val() + two().val())


