from typing import Callable, TypeVar, List

T = TypeVar('T')

def func1(a: str, b: Callable[[int, int], T]) -> T:
    pass

def func2 (a: str) -> "a string":
    return a

i:int = 10
def func3 () -> 3 + i:
    return 0
