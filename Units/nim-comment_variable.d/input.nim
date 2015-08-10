import streams, msgpack

type
  TA = object of RootObj
  TB = object of TA
    f: int

var
  a: ref TA
  b: ref TB

new(b)
a = b

echo stringify(pack(a)) #produces "[ ]", not "[ 0 ]" or '{ "f" : 0 }'
