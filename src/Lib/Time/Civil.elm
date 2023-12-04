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

import Integer as Z
import Lib.Math.Integer as Z
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
        , hour = Hours.fromInteger <| Z.zero
        , minute = Minutes.fromInteger <| Z.zero
        , second = Seconds.fromInteger <| Z.zero
        , millisecond = Milliseconds.fromInteger <| Z.zero
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
                |> Milliseconds.mul (Milliseconds.fromInteger Z.negativeOne)
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
            if Month.toInteger record.month |> Z.isLessThanOrEqual Z.two then
                Z.negativeOne

            else
                Z.zero

        numYears =
            Years.toInteger record.year
                |> Z.add yearAdjustment

        eraAdjustment =
            if Z.isNonNegative numYears then
                Z.zero

            else
                Z.fromSafeInt -399

        numEras =
            Z.add numYears eraAdjustment
                |> (\n -> Z.div n (Z.fromSafeInt 400))

        numEraYears =
            Z.fromSafeInt 400
                |> Z.mul numEras
                |> Z.sub numYears

        numMonths =
            Month.toInteger record.month

        monthAdjustment =
            Z.fromSafeInt <|
                if numMonths |> Z.isGreaterThan Z.two then
                    -3

                else
                    9

        numMonthsInDays =
            Z.add numMonths monthAdjustment
                |> Z.mul (Z.fromSafeInt 153)
                |> Z.add Z.two
                |> (\n -> Z.div n Z.five)

        numYearDays =
            Days.toInteger record.day
                |> Z.add Z.negativeOne
                |> Z.add numMonthsInDays

        numEraDays =
            let
                divEraYearsBy n =
                    Z.fromSafeInt n
                        |> Z.div numEraYears
            in
            Z.fromSafeInt 365
                |> Z.mul numEraYears
                |> Z.add (divEraYearsBy 4)
                |> Z.add (divEraYearsBy -100)
                |> Z.add numYearDays

        numDaysInMilliseconds =
            Z.fromSafeInt 146097
                |> Z.mul numEras
                |> Z.add numEraDays
                |> Z.add (Z.fromSafeInt -719468)
                |> Z.mul (Z.fromSafeInt (24 * 60 * 60 * 1000))

        numHoursInMilliseconds =
            Hours.toInteger record.hour
                |> Z.mul (Z.fromSafeInt (60 * 60 * 1000))

        numMinutesInMilliseconds =
            Minutes.toInteger record.minute
                |> Z.mul (Z.fromSafeInt (60 * 1000))

        numSecondsInMilliseconds =
            Seconds.toInteger record.second
                |> Z.mul (Z.fromSafeInt 1000)

        numMilliseconds =
            Milliseconds.toInteger record.millisecond
    in
    numDaysInMilliseconds
        |> Z.add numHoursInMilliseconds
        |> Z.add numMinutesInMilliseconds
        |> Z.add numSecondsInMilliseconds
        |> Z.add numMilliseconds
        |> Milliseconds.fromInteger
        |> Time.fromMilliseconds
