module Lib.Math.Integer exposing (div, eleven, mod, twelve)

import Integer as Z exposing (Integer)


eleven : Integer
eleven =
    Z.fromSafeInt 11


twelve : Integer
twelve =
    Z.fromSafeInt 12


div : Integer -> Integer -> Integer
div x y =
    x |> Z.divBy y |> Maybe.withDefault Z.zero


mod : Integer -> Integer -> Integer
mod x y =
    x |> Z.modBy y |> Maybe.withDefault Z.zero
