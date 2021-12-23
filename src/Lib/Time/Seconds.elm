module Lib.Time.Seconds exposing
    ( Seconds
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


type Seconds
    = Seconds BigInt



-- GENERAL HELPERS


toBigInt : Seconds -> BigInt
toBigInt (Seconds n) =
    n


fromBigInt : BigInt -> Seconds
fromBigInt =
    Seconds


fromInt : Int -> Seconds
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


map : (BigInt -> BigInt) -> Seconds -> Seconds
map f (Seconds a) =
    Seconds <| f a


map2 : (BigInt -> BigInt -> BigInt) -> Seconds -> Seconds -> Seconds
map2 f (Seconds a) (Seconds b) =
    Seconds <| f a b
