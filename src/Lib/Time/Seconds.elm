module Lib.Time.Seconds exposing
    ( Seconds
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


type Seconds
    = Seconds Integer



-- GENERAL HELPERS


toInteger : Seconds -> Integer
toInteger (Seconds n) =
    n


fromInteger : Integer -> Seconds
fromInteger =
    Seconds


fromInt : Int -> Seconds
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


map : (Integer -> Integer) -> Seconds -> Seconds
map f (Seconds a) =
    Seconds <| f a


map2 : (Integer -> Integer -> Integer) -> Seconds -> Seconds -> Seconds
map2 f (Seconds a) (Seconds b) =
    Seconds <| f a b
