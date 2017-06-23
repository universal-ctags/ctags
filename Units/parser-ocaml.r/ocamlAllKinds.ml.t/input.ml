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

let shall_appear () =
    let sub_not 1 = 2
    and shall_not_either fu = () in
    let nope = 3
and must_appear_also 4 = ()


let foo_function a b = (a, b)

class  fooClass =
object (self)
    val x = ()
    method fooMethod = x
end

exception ConnectionNotReachable
