type
  ttt = tuple[a:string,b:int,c:int,d:float]

proc unpack_type*(s: Stream, val: var StringTableRef)
proc unpack_type*(s: Stream, val: var float32)
proc unpack_type*[K,V](s: Stream, val: var OrderedTable[K,V])
proc unpack_type*[K,V](s: Stream, val: var TableRef[K,V])
