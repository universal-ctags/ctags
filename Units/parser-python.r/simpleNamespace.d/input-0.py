import types

tSA0 = types.SimpleNamespace(
    tSA0a="unknown",
    tSA0b="md5",
)

tsD= {
    "name": 1
}

tSA1 = types.SimpleNamespace(tSA1a = lambda x:  x + 1, tSA1b = 1, **tsD)

tx = types.SimpleNamespace
ty = types.SimpleNamespace.mro()

def tf():
    pass
