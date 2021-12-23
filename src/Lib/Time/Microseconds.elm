module Lib.Time.Microseconds exposing
    ( Microseconds
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


type Microseconds
    = Microseconds BigInt



-- GENERAL HELPERS


toBigInt : Microseconds -> BigInt
toBigInt (Microseconds n) =
    n


fromBigInt : BigInt -> Microseconds
fromBigInt =
    Microseconds


fromInt : Int -> Microseconds
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


map : (BigInt -> BigInt) -> Microseconds -> Microseconds
map f (Microseconds a) =
    Microseconds <| f a


map2 : (BigInt -> BigInt -> BigInt) -> Microseconds -> Microseconds -> Microseconds
map2 f (Microseconds a) (Microseconds b) =
    Microseconds <| f a b
