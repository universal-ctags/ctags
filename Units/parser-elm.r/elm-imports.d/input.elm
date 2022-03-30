module SomeMod exposing (..)

import PlainImport

import MyMod exposing
    ( map, foldl
    , Maybe, Possibly
    , Result(..)
    , MyList(Empty), Tree(Node, Value, Special) )

-- Allow a bit of looseness in module naming, even though the
-- compiler will reject it

import otherMod exposing (Coin)

-- Allow a dotted module name

import Dotted.name.Here exposing (Dot(Cons))

func x =
    42 + 24
