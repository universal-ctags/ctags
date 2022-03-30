port module Main exposing (..)

port outgoing : Enc.Value -> Cmd msg

port incoming : (Enc.Value -> msg) -> Sub msg

