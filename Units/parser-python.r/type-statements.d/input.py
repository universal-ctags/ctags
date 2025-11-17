from typing import TypeAlias

MyList0 = list[str]
MyList1: TypeAlias = list[str]
type MyList2 = list[str]
type ListOrSet[T] = list[T] | set[T]
