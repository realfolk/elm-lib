module Lib.Time.Nanoseconds exposing
    ( Nanoseconds
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


type Nanoseconds
    = Nanoseconds Integer



-- GENERAL HELPERS


toInteger : Nanoseconds -> Integer
toInteger (Nanoseconds n) =
    n


fromInteger : Integer -> Nanoseconds
fromInteger =
    Nanoseconds


fromInt : Int -> Nanoseconds
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


map : (Integer -> Integer) -> Nanoseconds -> Nanoseconds
map f (Nanoseconds a) =
    Nanoseconds <| f a


map2 : (Integer -> Integer -> Integer) -> Nanoseconds -> Nanoseconds -> Nanoseconds
map2 f (Nanoseconds a) (Nanoseconds b) =
    Nanoseconds <| f a b
