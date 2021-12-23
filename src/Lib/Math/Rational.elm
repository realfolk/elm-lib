module Lib.Math.Rational exposing
    ( Rational
    , denominator
    , fromBigInt
    , fromFloat
    , fromInt
    , numerator
    , toString
    )

import BigInt exposing (BigInt)
import Lib.Math.BigInt.Extra as BigInt


type Rational
    = Rational BigInt BigInt



-- ACCESSORS


numerator : Rational -> BigInt
numerator (Rational n _) =
    n


denominator : Rational -> BigInt
denominator (Rational _ d) =
    d



-- CONSTRUCTORS


fromInt : Int -> Int -> Rational
fromInt n d =
    fromBigInt (BigInt.fromInt n) (BigInt.fromInt d)


fromFloat : Int -> Float -> Rational
fromFloat decimalPlaces n =
    let
        whole =
            truncate n

        denom =
            10 ^ decimalPlaces

        remainderNumerator =
            (n - toFloat whole)
                * toFloat denom
                |> round

        numer =
            whole * denom + remainderNumerator
    in
    fromInt numer denom


fromBigInt : BigInt -> BigInt -> Rational
fromBigInt numer denom =
    let
        divisor =
            gcd numer denom

        g =
            if BigInt.isNegative denom then
                BigInt.negate divisor

            else
                divisor

        n =
            BigInt.div numer g

        d =
            BigInt.div denom g
    in
    Rational n d



-- CONVERTERS


toString : Rational -> String
toString (Rational n d) =
    if BigInt.isOne d then
        BigInt.toString n

    else
        BigInt.toString n ++ "/" ++ BigInt.toString d



-- HELPERS


gcd : BigInt -> BigInt -> BigInt
gcd a b =
    gcdHelper (BigInt.abs a) (BigInt.abs b)


gcdHelper : BigInt -> BigInt -> BigInt
gcdHelper a b =
    if BigInt.isZero b then
        a

    else
        gcdHelper b (Maybe.withDefault BigInt.zero <| BigInt.modBy b a)
