module Lib.Time.Microseconds exposing
    ( Microseconds
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


type Microseconds
    = Microseconds Integer



-- GENERAL HELPERS


toInteger : Microseconds -> Integer
toInteger (Microseconds n) =
    n


fromInteger : Integer -> Microseconds
fromInteger =
    Microseconds


fromInt : Int -> Microseconds
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


map : (Integer -> Integer) -> Microseconds -> Microseconds
map f (Microseconds a) =
    Microseconds <| f a


map2 : (Integer -> Integer -> Integer) -> Microseconds -> Microseconds -> Microseconds
map2 f (Microseconds a) (Microseconds b) =
    Microseconds <| f a b
