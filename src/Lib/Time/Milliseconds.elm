module Lib.Time.Milliseconds exposing
    ( Milliseconds
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


type Milliseconds
    = Milliseconds Integer



-- GENERAL HELPERS


toInteger : Milliseconds -> Integer
toInteger (Milliseconds n) =
    n


fromInteger : Integer -> Milliseconds
fromInteger =
    Milliseconds


fromInt : Int -> Milliseconds
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
    map2 Z.div



-- INTERNAL HELPERS


map : (Integer -> Integer) -> Milliseconds -> Milliseconds
map f (Milliseconds a) =
    Milliseconds <| f a


map2 : (Integer -> Integer -> Integer) -> Milliseconds -> Milliseconds -> Milliseconds
map2 f (Milliseconds a) (Milliseconds b) =
    Milliseconds <| f a b
