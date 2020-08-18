from typing import Callable, TypeVar, List, Tuple

T = TypeVar('T')

def func1(a: str, b: Callable[[int, int], T]) -> T:
    pass

def func2 (a: str) -> "a string":
    return a

i:int = 10
t: Tuple[str, ...] = ()

def func3 () -> 3 + i:
    return 0

# The type "Callable[[int], int]" given to "id" is not used well.
# See comments in parseVariable() of parser/python.c the difficulties.
id: Callable[[int], int] = lambda var1: var1
id2 = lambda var2: var2

id3: \
    Callable[[int], int] = \
        lambda var3: var3
