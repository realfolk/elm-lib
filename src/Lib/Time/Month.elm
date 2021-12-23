module Lib.Time.Month exposing
    ( Month(..)
    , fromBigInt
    , fromExternalMonth
    , fromInt
    , toBigInt
    , toExternalMonth
    , toInt
    , toShortString
    , toString
    )

import BigInt exposing (BigInt)
import Time


type Month
    = January
    | February
    | March
    | April
    | May
    | June
    | July
    | August
    | September
    | October
    | November
    | December


toString : Month -> String
toString month =
    case month of
        January ->
            "January"

        February ->
            "February"

        March ->
            "March"

        April ->
            "April"

        May ->
            "May"

        June ->
            "June"

        July ->
            "July"

        August ->
            "August"

        September ->
            "September"

        October ->
            "October"

        November ->
            "November"

        December ->
            "December"


toShortString : Month -> String
toShortString month =
    case month of
        January ->
            "Jan"

        February ->
            "Feb"

        March ->
            "Mar"

        April ->
            "Apr"

        May ->
            "May"

        June ->
            "Jun"

        July ->
            "Jul"

        August ->
            "Aug"

        September ->
            "Sep"

        October ->
            "Oct"

        November ->
            "Nov"

        December ->
            "Dec"


toBigInt : Month -> BigInt
toBigInt =
    toInt >> BigInt.fromInt


fromBigInt : BigInt -> Maybe Month
fromBigInt n =
    if n == BigInt.fromInt 1 then
        Just January

    else if n == BigInt.fromInt 2 then
        Just February

    else if n == BigInt.fromInt 3 then
        Just March

    else if n == BigInt.fromInt 4 then
        Just April

    else if n == BigInt.fromInt 5 then
        Just May

    else if n == BigInt.fromInt 6 then
        Just June

    else if n == BigInt.fromInt 7 then
        Just July

    else if n == BigInt.fromInt 8 then
        Just August

    else if n == BigInt.fromInt 9 then
        Just September

    else if n == BigInt.fromInt 10 then
        Just October

    else if n == BigInt.fromInt 11 then
        Just November

    else if n == BigInt.fromInt 12 then
        Just December

    else
        Nothing


toInt : Month -> Int
toInt month =
    case month of
        January ->
            1

        February ->
            2

        March ->
            3

        April ->
            4

        May ->
            5

        June ->
            6

        July ->
            7

        August ->
            8

        September ->
            9

        October ->
            10

        November ->
            11

        December ->
            12


fromInt : Int -> Maybe Month
fromInt =
    BigInt.fromInt >> fromBigInt


toExternalMonth : Month -> Time.Month
toExternalMonth month =
    case month of
        January ->
            Time.Jan

        February ->
            Time.Feb

        March ->
            Time.Mar

        April ->
            Time.Apr

        May ->
            Time.May

        June ->
            Time.Jun

        July ->
            Time.Jul

        August ->
            Time.Aug

        September ->
            Time.Sep

        October ->
            Time.Oct

        November ->
            Time.Nov

        December ->
            Time.Dec


fromExternalMonth : Time.Month -> Month
fromExternalMonth ext =
    case ext of
        Time.Jan ->
            January

        Time.Feb ->
            February

        Time.Mar ->
            March

        Time.Apr ->
            April

        Time.May ->
            May

        Time.Jun ->
            June

        Time.Jul ->
            July

        Time.Aug ->
            August

        Time.Sep ->
            September

        Time.Oct ->
            October

        Time.Nov ->
            November

        Time.Dec ->
            December
