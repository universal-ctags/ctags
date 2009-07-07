module ModuleFoo = struct
    type foobar =
          ConstructorFoo
        | ConstructorBar of int * char list
end

type 'a foorecord =
    { foofield : 'a;
     barfield : int;
     mutable foobarfield : list char -> int -> unit }

(* op redif *)
let (+-) a b =
    let aplus = a + b
    and aminus = a - b
    in
    (aplus, aminus)

let foo_function a b = (a, b)

class  fooClass =
object (self)
    val x = ()
    method fooMethod = x
end

