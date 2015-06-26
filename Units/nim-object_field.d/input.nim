type
  Fruits = enum
    Apple, Banana, Lemon, Durian, Rambutan

  Vehicle = object
    wheel, speed: int
    brand: string

  Animal = ref object of RootObj
    name: string
    speed: int

  Gradient* = ref object
      ID*, objID*: int
      a*, b*: RGBColor
      case gradType*: GradientType
      of GDT_LINEAR:
        axis* : Coord
      of GDT_RADIAL:
        radCoord*: CoordRadial

  anotherint = int
  inttoo = distinct int


proc isOperator*(tok: TToken, Kilo: var int): bool
proc parseAll*(p: var TParser, tonne: int): PNode
