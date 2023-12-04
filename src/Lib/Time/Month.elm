module Lib.Time.Month exposing
    ( Month(..)
    , fromExternalMonth
    , fromInt
    , fromInteger
    , toExternalMonth
    , toInt
    , toInteger
    , toShortString
    , toString
    )

import Integer as Z exposing (Integer)
import Lib.Math.Integer as Z
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


toInteger : Month -> Integer
toInteger =
    toInt >> Z.fromSafeInt


fromInteger : Integer -> Maybe Month
fromInteger z =
    if z == Z.one then
        Just January

    else if z == Z.two then
        Just February

    else if z == Z.three then
        Just March

    else if z == Z.four then
        Just April

    else if z == Z.five then
        Just May

    else if z == Z.six then
        Just June

    else if z == Z.seven then
        Just July

    else if z == Z.eight then
        Just August

    else if z == Z.nine then
        Just September

    else if z == Z.ten then
        Just October

    else if z == Z.eleven then
        Just November

    else if z == Z.twelve then
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
    Z.fromSafeInt >> fromInteger


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
