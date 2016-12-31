port module Main exposing (..)

import List
import Maybe exposing (withDefault)
import Json.Encode as Je


type Thing
    = One
    | Two Int


type Param a
    = Cons a
    | Other a


type alias Num =
    Int


port outward : String -> Cmd a


port inward : (b -> a) -> Sub a


foo : Int -> Int
foo a =
    a + 1


bar =
    let
        bas =
            1
    in
        bas
