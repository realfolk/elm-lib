module Test.Lib.Math.Rational exposing (suite)

import Expect
import Lib.Math.Rational as Rational
import Test exposing (..)


suite : Test
suite =
    describe "Lib.Math.Rational"
        [ constructorSuite
        ]


constructorSuite : Test
constructorSuite =
    describe "constructors"
        [ describe "fromInt"
            [ test "both numerator and denominator are positive" <|
                \_ ->
                    Rational.fromInt 2 4
                        |> Rational.toString
                        |> Expect.equal "1/2"
            , test "only numerator is positive" <|
                \_ ->
                    Rational.fromInt 2 -4
                        |> Rational.toString
                        |> Expect.equal "-1/2"
            , test "only denominator is positive" <|
                \_ ->
                    Rational.fromInt -2 4
                        |> Rational.toString
                        |> Expect.equal "-1/2"
            , test "both numerator and denominator are negative" <|
                \_ ->
                    Rational.fromInt -2 -4
                        |> Rational.toString
                        |> Expect.equal "1/2"
            ]
        , describe "fromFloat"
            [ test "0.1" <|
                \_ ->
                    Rational.fromFloat 1 0.1
                        |> Rational.toString
                        |> Expect.equal "1/10"
            , test "0.375" <|
                \_ ->
                    Rational.fromFloat 3 0.375
                        |> Rational.toString
                        |> Expect.equal "3/8"
            , test "0.5" <|
                \_ ->
                    Rational.fromFloat 1 0.5
                        |> Rational.toString
                        |> Expect.equal "1/2"
            , test "-1.125" <|
                \_ ->
                    Rational.fromFloat 3 -1.125
                        |> Rational.toString
                        |> Expect.equal "-9/8"
            ]
        ]
