module Lib.Time.Years exposing
    ( Years
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


type Years
    = Years Integer



-- GENERAL HELPERS


toInteger : Years -> Integer
toInteger (Years n) =
    n


fromInteger : Integer -> Years
fromInteger =
    Years


fromInt : Int -> Years
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


map : (Integer -> Integer) -> Years -> Years
map f (Years a) =
    Years <| f a


map2 : (Integer -> Integer -> Integer) -> Years -> Years -> Years
map2 f (Years a) (Years b) =
    Years <| f a b
