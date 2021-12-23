module Lib.Time.Hours exposing
    ( Hours
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


type Hours
    = Hours BigInt



-- GENERAL HELPERS


toBigInt : Hours -> BigInt
toBigInt (Hours n) =
    n


fromBigInt : BigInt -> Hours
fromBigInt =
    Hours


fromInt : Int -> Hours
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


map : (BigInt -> BigInt) -> Hours -> Hours
map f (Hours a) =
    Hours <| f a


map2 : (BigInt -> BigInt -> BigInt) -> Hours -> Hours -> Hours
map2 f (Hours a) (Hours b) =
    Hours <| f a b
