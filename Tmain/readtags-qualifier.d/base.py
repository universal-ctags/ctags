#
# ./ctags --fields='*' --extras='*' -o output.tags base.py
#
class Foo:
    def aq ():
        pass
    def aw ():
        pass
    def ae ():
        pass
    class A:
        pass
class Bar (Foo):
    def bq ():
        pass
    def bw ():
        pass
    class B:
        pass

class Baz (Foo): 
    def bq ():
        pass
    def bw ():
        pass
    class C:
        pass
            
