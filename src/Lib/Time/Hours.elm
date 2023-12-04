module Lib.Time.Hours exposing
    ( Hours
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


type Hours
    = Hours Integer



-- GENERAL HELPERS


toInteger : Hours -> Integer
toInteger (Hours n) =
    n


fromInteger : Integer -> Hours
fromInteger =
    Hours


fromInt : Int -> Hours
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


map : (Integer -> Integer) -> Hours -> Hours
map f (Hours a) =
    Hours <| f a


map2 : (Integer -> Integer -> Integer) -> Hours -> Hours -> Hours
map2 f (Hours a) (Hours b) =
    Hours <| f a b
