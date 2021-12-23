module Lib.Time.Days exposing
    ( Days
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


type Days
    = Days BigInt



-- GENERAL HELPERS


toBigInt : Days -> BigInt
toBigInt (Days n) =
    n


fromBigInt : BigInt -> Days
fromBigInt =
    Days


fromInt : Int -> Days
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


map : (BigInt -> BigInt) -> Days -> Days
map f (Days a) =
    Days <| f a


map2 : (BigInt -> BigInt -> BigInt) -> Days -> Days -> Days
map2 f (Days a) (Days b) =
    Days <| f a b
