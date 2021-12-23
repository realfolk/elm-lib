module Lib.Time.Minutes exposing
    ( Minutes
    , add
    , div
    , fromBigInt
    , fromInt
    , mul
    , sub
    , toBigInt
    )

import BigInt exposing (BigInt)



-- TYPES


type Minutes
    = Minutes BigInt



-- GENERAL HELPERS


toBigInt : Minutes -> BigInt
toBigInt (Minutes n) =
    n


fromBigInt : BigInt -> Minutes
fromBigInt =
    Minutes


fromInt : Int -> Minutes
fromInt =
    BigInt.fromInt >> fromBigInt



-- ARITHMETIC


add =
    map2 BigInt.add


sub =
    map2 BigInt.sub


mul =
    map2 BigInt.mul


div =
    map2 BigInt.div



-- INTERNAL HELPERS


map : (BigInt -> BigInt) -> Minutes -> Minutes
map f (Minutes a) =
    Minutes <| f a


map2 : (BigInt -> BigInt -> BigInt) -> Minutes -> Minutes -> Minutes
map2 f (Minutes a) (Minutes b) =
    Minutes <| f a b
