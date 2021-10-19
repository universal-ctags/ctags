from typing import List, Union
v: float | str = 'a'

# Taken from https://www.python.org/dev/peps/pep-0604/
def f(list: List[int|str], param: int | None) -> float|str:
    pass
