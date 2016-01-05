def f (
        # IGNORE THIS COMMENT
        a):
    pass
def g (
        # IGNORE THIS COMMENT
        a = 3):
    pass

x = {
    'y': 1
}

z = [
    x
]

# https://github.com/universal-ctags/ctags/issues/750#issuecomment-169002893
# Reported by @strokirk
class Foo(object):
    def bar(self,
            default=None):
        pass
    baz = dict(
        argument=None,
    )

variable = dict(
    keyword=None,
)
