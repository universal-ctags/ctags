type
  Point2[T] = tuple[x, y: T]

  TProperty*[T] = object of TObject
    getProc: proc(property: TProperty[T]): T {.nimcall.}
    setProc: proc(property: var TProperty[T], value: T) {.nimcall.}
    value: T
    ValueChanged*: TEventHandler
    Binders: seq[TProperty[T]]
    EEmitter: TEventEmitter
  # Not a descriptive name but it was that or TPropertyValueChangeEventArgs.
  TValueEventArgs[T] = object of TEventArgs
    Property*: TProperty[T]
