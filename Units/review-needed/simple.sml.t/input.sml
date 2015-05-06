(* This is a comment *)

abstype abstype_name

datatype suit = Spades | Hearts | Diamonds | Clubs

exception Change

fun dist (x:real, y:real):real = sqrt (x*x + y*y)

fun checked_factorial n =
    if n < 0 then
	raise Factorial
    else if n=0 then
	1
	 else
	     n * checked_factorial (n-1)

exception Factorial

local
    fun fact 0 = 1
      | fact n = n * fact (n-1)
in
    fun checked_factorial n =
	if n >= 0 then
	    fact n
	else
	    raise Factorial
end 

functor Matcher (structure RegExp : REGEXP) :> MATCHER = struct

    structure RegExp = RegExp

    open RegExp

    fun match_is Zero cs k = false
      | match_is One cs k = k cs
      | match_is (Char c) nil _ = false
      | match_is (Char c) (c'::cs) k = (c=c') andalso (k cs)
      | match_is (Plus (r1, r2)) cs k =
	(match_is r1 cs k) orelse (match_is r2 cs k)
      | match_is (Times (r1, r2)) cs k =
	match_is r1 cs (fn cs' => match_is r2 cs' k)
      | match_is (r as Star r1) cs k =
	(k cs) orelse match_is r1 cs (fn cs' => match_is r cs' k)

    fun match regexp string =
	match_is regexp (String.explode string)
	(fn nil => true | _ => false)

end

signature SUSP = sig
  type 'a susp
  val force : 'a susp -> 'a
  val delay : (unit -> 'a) -> 'a susp
end

structure Susp :> SUSP = struct
  type 'a susp = unit -> 'a
  fun force t = t ()
  fun delay (t : 'a susp) =
      let
          exception Impossible
          val memo : 'a susp ref = ref (fn () => raise Impossible)
          fun t' () =
              let val r = t () in memo := (fn () => r); r end
      in
          memo := t';
          fn () => (!memo)()
      end
  fun loopback f =
      let
	  exception Circular
	  val r = ref (fn () => raise Circular)
	  fun t () = force (!r)
      in
	  r := f t ; t
      end
end

type hyperlink = { protocol : string, address : string, display : string }

val dist : real * real -> real = fn (x:real, y:real) => sqrt (x*x + y*y)
