module Lib.Math.Rational exposing
    ( Rational
    , denominator
    , fromFloat
    , fromInt
    , fromInteger
    , numerator
    , toString
    )

import Integer as Z exposing (Integer)
import Lib.Math.Integer as Z


type Rational
    = Rational Integer Integer



-- ACCESSORS


numerator : Rational -> Integer
numerator (Rational n _) =
    n


denominator : Rational -> Integer
denominator (Rational _ d) =
    d



-- CONSTRUCTORS


fromInt : Int -> Int -> Rational
fromInt n d =
    fromInteger (Z.fromSafeInt n) (Z.fromSafeInt d)


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


fromInteger : Integer -> Integer -> Rational
fromInteger numer denom =
    let
        divisor =
            gcd numer denom

        g =
            if Z.isNegative denom then
                Z.negate divisor

            else
                divisor

        n =
            Z.safeDiv numer g

        d =
            Z.safeDiv denom g
    in
    Rational n d



-- CONVERTERS


toString : Rational -> String
toString (Rational n d) =
    if d == Z.one then
        Z.toString n

    else
        Z.toString n ++ "/" ++ Z.toString d



-- HELPERS


gcd : Integer -> Integer -> Integer
gcd a b =
    gcdHelper (Z.abs a) (Z.abs b)


gcdHelper : Integer -> Integer -> Integer
gcdHelper a b =
    if Z.isZero b then
        a

    else
        gcdHelper b (Z.safeMod a b)
