module Lib.Time.Weeks exposing
    ( Weeks
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


type Weeks
    = Weeks Integer



-- GENERAL HELPERS


toInteger : Weeks -> Integer
toInteger (Weeks n) =
    n


fromInteger : Integer -> Weeks
fromInteger =
    Weeks


fromInt : Int -> Weeks
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


map : (Integer -> Integer) -> Weeks -> Weeks
map f (Weeks a) =
    Weeks <| f a


map2 : (Integer -> Integer -> Integer) -> Weeks -> Weeks -> Weeks
map2 f (Weeks a) (Weeks b) =
    Weeks <| f a b
