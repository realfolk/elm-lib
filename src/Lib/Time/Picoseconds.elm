module Lib.Time.Picoseconds exposing
    ( Picoseconds
    , add
    , div
    , fromInt
    , fromInteger
    , mul
    , sub
    , toInteger
    )

import Integer as Z exposing (Integer)
import Lib.Math.Integer as Z



-- TYPES


type Picoseconds
    = Picoseconds Integer



-- GENERAL HELPERS


toInteger : Picoseconds -> Integer
toInteger (Picoseconds n) =
    n


fromInteger : Integer -> Picoseconds
fromInteger =
    Picoseconds


fromInt : Int -> Picoseconds
fromInt =
    Z.fromSafeInt >> fromInteger



-- ARITHMETIC


add =
    map2 Z.add


sub =
    map2 Z.sub


mul =
    map2 Z.mul


div =
    map2 Z.safeDiv



-- INTERNAL HELPERS


map : (Integer -> Integer) -> Picoseconds -> Picoseconds
map f (Picoseconds a) =
    Picoseconds <| f a


map2 : (Integer -> Integer -> Integer) -> Picoseconds -> Picoseconds -> Picoseconds
map2 f (Picoseconds a) (Picoseconds b) =
    Picoseconds <| f a b
