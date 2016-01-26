# triple start string immediately after a normal string not detected

def f1():
    ''"""
    The string above was not detected as triple start string,
    but the one below instead.
    """
    print "f1"

def f2():
    ''"""
    The string above was then detected as end string,
    and the one below as start string again.
    """
    print "f2"

def f3():
    """
    The string below is prepared so that ctags with the bug does not start a
    new triple string. For a clean precondition for the next test.
    ''"""
    print "f3"

# normal string immediately after a normal string not detected

''" import os\
"

""' def fX():\
'
