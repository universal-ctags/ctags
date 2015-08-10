type
  TDict[TK, TV] = object
    k: TK
    v: TV
  PDict[TK, TV] = ref TDict[TK, TV]

type
  TObj = object
    x: int

  Sortable = concept x, y
    (x < y) is bool

  ObjectContainer = concept C
    C.len is Ordinal
    for v in items(C):
      v.type is tuple|object

type
  Container[T] = concept c
    c.len is Ordinal
    items(c) is iterator
    for value in c:
      type(value) is T

type RNG* = concept var rng
  rng.randomUint32() is uint32
