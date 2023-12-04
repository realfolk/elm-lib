module Lib.Time.Weekday exposing
    ( Weekday(..)
    , fromExternalWeekday
    , fromInt
    , fromInteger
    , toExternalWeekday
    , toInt
    , toInteger
    , toShortString
    , toString
    )

import Integer as Z exposing (Integer)
import Time


type Weekday
    = Monday
    | Tuesday
    | Wednesday
    | Thursday
    | Friday
    | Saturday
    | Sunday


toString : Weekday -> String
toString weekday =
    case weekday of
        Monday ->
            "Monday"

        Tuesday ->
            "Tuesday"

        Wednesday ->
            "Wednesday"

        Thursday ->
            "Thursday"

        Friday ->
            "Friday"

        Saturday ->
            "Saturday"

        Sunday ->
            "Sunday"


toShortString : Weekday -> String
toShortString weekday =
    case weekday of
        Monday ->
            "Mon"

        Tuesday ->
            "Tue"

        Wednesday ->
            "Wed"

        Thursday ->
            "Thu"

        Friday ->
            "Fri"

        Saturday ->
            "Sat"

        Sunday ->
            "Sun"


toInteger : Weekday -> Integer
toInteger =
    toInt >> Z.fromSafeInt


fromInteger : Integer -> Maybe Weekday
fromInteger n =
    if n == Z.one then
        Just Monday

    else if n == Z.two then
        Just Tuesday

    else if n == Z.three then
        Just Wednesday

    else if n == Z.four then
        Just Thursday

    else if n == Z.five then
        Just Friday

    else if n == Z.six then
        Just Saturday

    else if n == Z.seven then
        Just Sunday

    else
        Nothing


toInt : Weekday -> Int
toInt weekday =
    case weekday of
        Monday ->
            1

        Tuesday ->
            2

        Wednesday ->
            3

        Thursday ->
            4

        Friday ->
            5

        Saturday ->
            6

        Sunday ->
            7


fromInt : Int -> Maybe Weekday
fromInt =
    Z.fromSafeInt >> fromInteger


toExternalWeekday : Weekday -> Time.Weekday
toExternalWeekday weekday =
    case weekday of
        Monday ->
            Time.Mon

        Tuesday ->
            Time.Tue

        Wednesday ->
            Time.Wed

        Thursday ->
            Time.Thu

        Friday ->
            Time.Fri

        Saturday ->
            Time.Sat

        Sunday ->
            Time.Sun


fromExternalWeekday : Time.Weekday -> Weekday
fromExternalWeekday ext =
    case ext of
        Time.Mon ->
            Monday

        Time.Tue ->
            Tuesday

        Time.Wed ->
            Wednesday

        Time.Thu ->
            Thursday

        Time.Fri ->
            Friday

        Time.Sat ->
            Saturday

        Time.Sun ->
            Sunday
