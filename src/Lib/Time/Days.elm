module Lib.Time.Days exposing
    ( Days
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


type Days
    = Days Integer



-- GENERAL HELPERS


toInteger : Days -> Integer
toInteger (Days n) =
    n


fromInteger : Integer -> Days
fromInteger =
    Days


fromInt : Int -> Days
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


map : (Integer -> Integer) -> Days -> Days
map f (Days a) =
    Days <| f a


map2 : (Integer -> Integer -> Integer) -> Days -> Days -> Days
map2 f (Days a) (Days b) =
    Days <| f a b
