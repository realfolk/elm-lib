module Lib.Math.Integer exposing
    ( eleven
    , safeDiv
    , safeDivBy
    , safeMod
    , safeModBy
    , safePow
    , safePow10
    , twelve
    )

import Integer as Z exposing (Integer)
import Natural as N



-- CONSTANTS


eleven : Integer
eleven =
    Z.fromSafeInt 11


twelve : Integer
twelve =
    Z.fromSafeInt 12



-- ARITHMETIC


safeDivBy : Integer -> Integer -> Integer
safeDivBy divisor dividend =
    dividend |> Z.divBy divisor |> Maybe.withDefault Z.zero


safeDiv : Integer -> Integer -> Integer
safeDiv dividend divisor =
    dividend |> safeDivBy divisor


safeModBy : Integer -> Integer -> Integer
safeModBy divisor dividend =
    dividend |> Z.modBy divisor |> Maybe.withDefault Z.zero


safeMod : Integer -> Integer -> Integer
safeMod dividend divisor =
    dividend |> safeModBy divisor


safePow : Integer -> Int -> Integer
safePow z =
    Z.exp z << N.fromSafeInt


safePow10 : Int -> Integer
safePow10 =
    safePow Z.ten
