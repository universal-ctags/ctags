import algorithm, tables

const
  STDMAP* = [
    0x2202, 0x2202, 0x123]
  MACMAP = [123,123]
  WINMAP = [123,123]

var col = initOrderedTable[int, int]();

col.sort(proc(x,y: tuple[key: int, val: int]):int = cmp(x, y) )

proc abc(x,y: tuple[key: int, val: int]) = discard

proc pack_type*[T: tuple|object](s: int, val: T) = discard
proc pack_type*[T](y: int, val: ptr T) = discard
proc unpack_ext*(s: int): tuple[exttype:uint8, len: int] = discard
