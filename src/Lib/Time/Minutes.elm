module Lib.Time.Minutes exposing
    ( Minutes
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


type Minutes
    = Minutes Integer



-- GENERAL HELPERS


toInteger : Minutes -> Integer
toInteger (Minutes n) =
    n


fromInteger : Integer -> Minutes
fromInteger =
    Minutes


fromInt : Int -> Minutes
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


map : (Integer -> Integer) -> Minutes -> Minutes
map f (Minutes a) =
    Minutes <| f a


map2 : (Integer -> Integer -> Integer) -> Minutes -> Minutes -> Minutes
map2 f (Minutes a) (Minutes b) =
    Minutes <| f a b
