var cATup, cBTup: tuple[x: int, ha: Concrete]

TAny = object
  case kind: TAnyKind
  of nkInt: intVal: int
  of nkFloat: floatVal: float
  of nkString: strVal: string

proc ugh: seq[tuple[x: string, c: int]] =
  result = @[("abc", 232)]

var ret: seq[tuple[name: string, a: TAny]] = @[]

proc fannkuch (n: int): int =
    var
        count: seq[int]
        maxFlips = 0
        m        = n-1
        r        = n
        check    = 0
        perm1: seq[int]
        perm:  seq[int]
