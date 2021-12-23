module Lib.Time.Civil exposing
    ( Civil
    , date, dateAndTime, toTime, fromTime
    , getZone, getZoneOffset
    )

{-| This module implements a `Civil` type to represent time local to a specific timezone. Its uses the algorithm described by Howard Hinnant in his paper, [chrono-Compatible Low-Level Date Algorithms](http://howardhinnant.github.io/date_algorithms.html#How%20can%20I%20confirm%20that%20your%20assertions%20about%20%3Ccode%3Echrono%3C/code%3E%20compatibility%20are%20correct?). These functions were also influenced by the [Adrian/Ribao/elm-derberos-date](https://github.com/AdrianRibao/elm-derberos-date/blob/1.2.3/src/Derberos/Date/Core.elm) Elm package, specifically `getZoneOffset` and the internal function, `toTimeWithoutZoneAdjustment`.

The motivation behind this module stems from the fact that `elm/time` exports an opaque `Zone` type. As a result, it is difficult to calculate a timezone's offset from UTC given a `Zone`. The `Civil` type enables the calculation of a timezone's offset in `Milliseconds` as implemented in `getZoneOffset`.


# Types

@docs Civil


# General Helpers

@docs date, dateAndTime, toTime, fromTime


# Accessors

@docs getZone, getZoneOffset

-}

import BigInt
import Lib.Time as Time exposing (Time)
import Lib.Time.Days as Days exposing (Days)
import Lib.Time.Hours as Hours exposing (Hours)
import Lib.Time.Milliseconds as Milliseconds exposing (Milliseconds)
import Lib.Time.Minutes as Minutes exposing (Minutes)
import Lib.Time.Month as Month exposing (Month)
import Lib.Time.Seconds as Seconds exposing (Seconds)
import Lib.Time.Years as Years exposing (Years)



-- TYPES


type Civil
    = Civil
        { year : Years
        , month : Month
        , day : Days
        , hour : Hours
        , minute : Minutes
        , second : Seconds
        , millisecond : Milliseconds
        , zone : Time.Zone
        }



-- GENERAL HELPERS


date : Time.Zone -> Years -> Month -> Days -> Civil
date zone year month day =
    Civil
        { year = year
        , month = month
        , day = day
        , hour = Hours.fromBigInt <| BigInt.fromInt 0
        , minute = Minutes.fromBigInt <| BigInt.fromInt 0
        , second = Seconds.fromBigInt <| BigInt.fromInt 0
        , millisecond = Milliseconds.fromBigInt <| BigInt.fromInt 0
        , zone = zone
        }


dateAndTime :
    Time.Zone
    -> Years
    -> Month
    -> Days
    -> Hours
    -> Minutes
    -> Seconds
    -> Milliseconds
    -> Civil
dateAndTime zone year month day hour minute second millisecond =
    Civil
        { year = year
        , month = month
        , day = day
        , hour = hour
        , minute = minute
        , second = second
        , millisecond = millisecond
        , zone = zone
        }


toTime : Civil -> Time
toTime ((Civil record) as civil) =
    let
        unadjustedTime =
            toTimeWithoutZoneAdjustment civil

        zoneOffset =
            getZoneOffset record.zone
                |> Milliseconds.mul (Milliseconds.fromBigInt (BigInt.fromInt -1))
    in
    Time.addMilliseconds zoneOffset unadjustedTime


fromTime : Time.Zone -> Time -> Civil
fromTime zone time =
    Civil
        { year = Time.getYear zone time
        , month = Time.getMonth zone time
        , day = Time.getDayOfMonth zone time
        , hour = Time.getHour zone time
        , minute = Time.getMinute zone time
        , second = Time.getSecond zone time
        , millisecond = Time.getMillisecond zone time
        , zone = zone
        }



-- ACCESSORS


getZone : Civil -> Time.Zone
getZone (Civil record) =
    record.zone


getZoneOffset : Time.Zone -> Milliseconds
getZoneOffset zone =
    let
        utcTime =
            Time.fromMilliseconds <| Milliseconds.fromInt 0

        utcMilliseconds =
            Time.toMilliseconds utcTime

        localMilliseconds =
            fromTime zone utcTime
                |> toTimeWithoutZoneAdjustment
                |> Time.toMilliseconds
    in
    Milliseconds.sub localMilliseconds utcMilliseconds



-- INTERNAL HELPERS


toTimeWithoutZoneAdjustment : Civil -> Time
toTimeWithoutZoneAdjustment (Civil record) =
    let
        yearAdjustment =
            BigInt.fromInt <|
                if BigInt.lte (Month.toBigInt record.month) (BigInt.fromInt 2) then
                    -1

                else
                    0

        numYears =
            Years.toBigInt record.year
                |> BigInt.add yearAdjustment

        eraAdjustment =
            BigInt.fromInt <|
                if BigInt.gte numYears (BigInt.fromInt 0) then
                    0

                else
                    -399

        numEras =
            BigInt.add numYears eraAdjustment
                |> (\n -> BigInt.div n (BigInt.fromInt 400))

        numEraYears =
            BigInt.fromInt 400
                |> BigInt.mul numEras
                |> BigInt.sub numYears

        numMonths =
            Month.toBigInt record.month

        monthAdjustment =
            BigInt.fromInt <|
                if BigInt.gt numMonths (BigInt.fromInt 2) then
                    -3

                else
                    9

        numMonthsInDays =
            BigInt.add numMonths monthAdjustment
                |> BigInt.mul (BigInt.fromInt 153)
                |> BigInt.add (BigInt.fromInt 2)
                |> (\n -> BigInt.div n (BigInt.fromInt 5))

        numYearDays =
            Days.toBigInt record.day
                |> BigInt.add (BigInt.fromInt -1)
                |> BigInt.add numMonthsInDays

        numEraDays =
            let
                divEraYearsBy n =
                    BigInt.fromInt n
                        |> BigInt.div numEraYears
            in
            BigInt.fromInt 365
                |> BigInt.mul numEraYears
                |> BigInt.add (divEraYearsBy 4)
                |> BigInt.add (divEraYearsBy -100)
                |> BigInt.add numYearDays

        numDaysInMilliseconds =
            BigInt.fromInt 146097
                |> BigInt.mul numEras
                |> BigInt.add numEraDays
                |> BigInt.add (BigInt.fromInt -719468)
                |> BigInt.mul (BigInt.fromInt (24 * 60 * 60 * 1000))

        numHoursInMilliseconds =
            Hours.toBigInt record.hour
                |> BigInt.mul (BigInt.fromInt (60 * 60 * 1000))

        numMinutesInMilliseconds =
            Minutes.toBigInt record.minute
                |> BigInt.mul (BigInt.fromInt (60 * 1000))

        numSecondsInMilliseconds =
            Seconds.toBigInt record.second
                |> BigInt.mul (BigInt.fromInt 1000)

        numMilliseconds =
            Milliseconds.toBigInt record.millisecond
    in
    numDaysInMilliseconds
        |> BigInt.add numHoursInMilliseconds
        |> BigInt.add numMinutesInMilliseconds
        |> BigInt.add numSecondsInMilliseconds
        |> BigInt.add numMilliseconds
        |> Milliseconds.fromBigInt
        |> Time.fromMilliseconds
