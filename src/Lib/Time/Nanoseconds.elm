module Lib.Time.Nanoseconds exposing
    ( Nanoseconds
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


type Nanoseconds
    = Nanoseconds BigInt



-- GENERAL HELPERS


toBigInt : Nanoseconds -> BigInt
toBigInt (Nanoseconds n) =
    n


fromBigInt : BigInt -> Nanoseconds
fromBigInt =
    Nanoseconds


fromInt : Int -> Nanoseconds
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


map : (BigInt -> BigInt) -> Nanoseconds -> Nanoseconds
map f (Nanoseconds a) =
    Nanoseconds <| f a


map2 : (BigInt -> BigInt -> BigInt) -> Nanoseconds -> Nanoseconds -> Nanoseconds
map2 f (Nanoseconds a) (Nanoseconds b) =
    Nanoseconds <| f a b
