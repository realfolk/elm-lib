module Lib.Time.Milliseconds exposing
    ( Milliseconds
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


type Milliseconds
    = Milliseconds BigInt



-- GENERAL HELPERS


toBigInt : Milliseconds -> BigInt
toBigInt (Milliseconds n) =
    n


fromBigInt : BigInt -> Milliseconds
fromBigInt =
    Milliseconds


fromInt : Int -> Milliseconds
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


map : (BigInt -> BigInt) -> Milliseconds -> Milliseconds
map f (Milliseconds a) =
    Milliseconds <| f a


map2 : (BigInt -> BigInt -> BigInt) -> Milliseconds -> Milliseconds -> Milliseconds
map2 f (Milliseconds a) (Milliseconds b) =
    Milliseconds <| f a b
