type
  TClassOfTCustomObject {.pure, inheritable.} = object
    base* : ptr TClassOfTCustomObject
    className* : string
  TClassOfTobj = object of TClassOfTCustomObject
    nil
  TCustomObject = ref object {.inheritable.}
    class* : ptr TClassOfTCustomObject
  TObj = ref object of TCustomObject
    data: int