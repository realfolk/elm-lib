module Lib.Time.Years exposing
    ( Years
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


type Years
    = Years BigInt



-- GENERAL HELPERS


toBigInt : Years -> BigInt
toBigInt (Years n) =
    n


fromBigInt : BigInt -> Years
fromBigInt =
    Years


fromInt : Int -> Years
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


map : (BigInt -> BigInt) -> Years -> Years
map f (Years a) =
    Years <| f a


map2 : (BigInt -> BigInt -> BigInt) -> Years -> Years -> Years
map2 f (Years a) (Years b) =
    Years <| f a b
