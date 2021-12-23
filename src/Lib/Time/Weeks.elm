module Lib.Time.Weeks exposing
    ( Weeks
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


type Weeks
    = Weeks BigInt



-- GENERAL HELPERS


toBigInt : Weeks -> BigInt
toBigInt (Weeks n) =
    n


fromBigInt : BigInt -> Weeks
fromBigInt =
    Weeks


fromInt : Int -> Weeks
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


map : (BigInt -> BigInt) -> Weeks -> Weeks
map f (Weeks a) =
    Weeks <| f a


map2 : (BigInt -> BigInt -> BigInt) -> Weeks -> Weeks -> Weeks
map2 f (Weeks a) (Weeks b) =
    Weeks <| f a b
