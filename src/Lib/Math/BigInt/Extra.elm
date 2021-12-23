module Lib.Math.BigInt.Extra exposing
    ( isNegative
    , isOne
    , isPositive
    , isZero
    , one
    , ten
    , zero
    )

import BigInt exposing (BigInt)



-- CONSTANTS


zero : BigInt
zero =
    BigInt.fromInt 0


one : BigInt
one =
    BigInt.fromInt 1


ten : BigInt
ten =
    BigInt.fromInt 10



-- PREDICATES


isZero : BigInt -> Bool
isZero n =
    BigInt.compare n zero == EQ


isOne : BigInt -> Bool
isOne n =
    BigInt.compare n one == EQ


isNegative : BigInt -> Bool
isNegative n =
    BigInt.compare n zero == LT


isPositive : BigInt -> Bool
isPositive n =
    BigInt.compare n zero == GT
