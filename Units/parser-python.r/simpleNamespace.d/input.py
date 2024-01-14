from types import SimpleNamespace

SA0 = SimpleNamespace(
    SA0a="unknown",
    SA0b="md5",
)

sD= {
    "name": 1
}

SA1 = SimpleNamespace(SA1a = lambda x:  x + 1, SA1b = 1, **sD)

x = SimpleNamespace
y = SimpleNamespace.mro()

def f():
    pass
