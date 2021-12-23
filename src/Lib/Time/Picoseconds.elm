module Lib.Time.Picoseconds exposing
    ( Picoseconds
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


type Picoseconds
    = Picoseconds BigInt



-- GENERAL HELPERS


toBigInt : Picoseconds -> BigInt
toBigInt (Picoseconds n) =
    n


fromBigInt : BigInt -> Picoseconds
fromBigInt =
    Picoseconds


fromInt : Int -> Picoseconds
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


map : (BigInt -> BigInt) -> Picoseconds -> Picoseconds
map f (Picoseconds a) =
    Picoseconds <| f a


map2 : (BigInt -> BigInt -> BigInt) -> Picoseconds -> Picoseconds -> Picoseconds
map2 f (Picoseconds a) (Picoseconds b) =
    Picoseconds <| f a b
