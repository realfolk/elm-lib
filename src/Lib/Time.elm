module Lib.Time exposing
    ( Add
    , Time
    , Zone
    , addDays
    , addHours
    , addMicroseconds
    , addMilliseconds
    , addMinutes
    , addNanoseconds
    , addPicoseconds
    , addSeconds
    , addWeeks
    , compare
    , customZone
    , every
    , fromDays
    , fromHours
    , fromMicroseconds
    , fromMilliseconds
    , fromMinutes
    , fromNanoseconds
    , fromPicoseconds
    , fromPosix
    , fromSeconds
    , fromWeeks
    , getDayOfMonth
    , getHour
    , getMillisecond
    , getMinute
    , getMonth
    , getSecond
    , getTimeZone
    , getWeekday
    , getYear
    , here
    , millisToPosix
    , now
    , posixToMillis
    , toDays
    , toHours
    , toMicroseconds
    , toMilliseconds
    , toMinutes
    , toNanoseconds
    , toPicoseconds
    , toPosix
    , toSeconds
    , toWeeks
    , utc
    )

import Integer as Z exposing (Integer)
import Lib.Math.Integer as Z
import Lib.Task as Task
import Lib.Time.Days as Days exposing (Days)
import Lib.Time.Hours as Hours exposing (Hours)
import Lib.Time.Microseconds as Microseconds exposing (Microseconds)
import Lib.Time.Milliseconds as Milliseconds exposing (Milliseconds)
import Lib.Time.Minutes as Minutes exposing (Minutes)
import Lib.Time.Month as Month exposing (Month)
import Lib.Time.Nanoseconds as Nanoseconds exposing (Nanoseconds)
import Lib.Time.Picoseconds as Picoseconds exposing (Picoseconds)
import Lib.Time.Seconds as Seconds exposing (Seconds)
import Lib.Time.Weekday as Weekday exposing (Weekday)
import Lib.Time.Weeks as Weeks exposing (Weeks)
import Lib.Time.Years as Years exposing (Years)
import Natural as N
import Task exposing (Task)
import Time



-- TIME


type Time
    = Time Picoseconds


now : Task x Time
now =
    Task.map fromPosix Time.now


{-| A `Sub` that emits a `msg` derived from the current `Time` every `n : Float` milliseconds.
-}
every : Float -> (Time -> msg) -> Sub msg
every n f =
    Time.every n (fromPosix >> f)



-- TIME ZONES


type Zone
    = Zone Time.Zone



-- TIME ZONES


utc : Zone
utc =
    Zone Time.utc


here : Task x Zone
here =
    Task.map Zone Time.here


customZone : Int -> List { start : Int, offset : Int } -> Zone
customZone offset exceptions =
    Zone <| Time.customZone offset exceptions


getTimeZone : (Zone -> msg) -> Cmd msg
getTimeZone toMsg =
    Task.safely (toMsg utc) toMsg here



-- ACCESSORS


getYear : Zone -> Time -> Years
getYear (Zone zone) time =
    toPosix time
        |> Time.toYear zone
        |> Z.fromSafeInt
        |> Years.fromInteger


getMonth : Zone -> Time -> Month
getMonth (Zone zone) time =
    toPosix time
        |> Time.toMonth zone
        |> Month.fromExternalMonth


getWeekday : Zone -> Time -> Weekday
getWeekday (Zone zone) time =
    toPosix time
        |> Time.toWeekday zone
        |> Weekday.fromExternalWeekday


getDayOfMonth : Zone -> Time -> Days
getDayOfMonth (Zone zone) time =
    toPosix time
        |> Time.toDay zone
        |> Z.fromSafeInt
        |> Days.fromInteger


getHour : Zone -> Time -> Hours
getHour (Zone zone) time =
    toPosix time
        |> Time.toHour zone
        |> Z.fromSafeInt
        |> Hours.fromInteger


getMinute : Zone -> Time -> Minutes
getMinute (Zone zone) time =
    toPosix time
        |> Time.toMinute zone
        |> Z.fromSafeInt
        |> Minutes.fromInteger


getSecond : Zone -> Time -> Seconds
getSecond (Zone zone) time =
    toPosix time
        |> Time.toSecond zone
        |> Z.fromSafeInt
        |> Seconds.fromInteger


getMillisecond : Zone -> Time -> Milliseconds
getMillisecond (Zone zone) time =
    toPosix time
        |> Time.toMillis zone
        |> Z.fromSafeInt
        |> Milliseconds.fromInteger



-- COMPARISON


compare : Time -> Time -> Order
compare t1 t2 =
    let
        x =
            toPicoseconds t1 |> Picoseconds.toInteger

        y =
            toPicoseconds t2 |> Picoseconds.toInteger
    in
    Z.compare x y



-- ARITHMETIC


type alias Add units =
    units -> Time -> Time


addWeeks : Add Weeks
addWeeks =
    add fromWeeks


addDays : Add Days
addDays =
    add fromDays


addHours : Add Hours
addHours =
    add fromHours


addMinutes : Add Minutes
addMinutes =
    add fromMinutes


addSeconds : Add Seconds
addSeconds =
    add fromSeconds


addMilliseconds : Add Milliseconds
addMilliseconds =
    add fromMilliseconds


addMicroseconds : Add Microseconds
addMicroseconds =
    add fromMicroseconds


addNanoseconds : Add Nanoseconds
addNanoseconds =
    add fromNanoseconds


addPicoseconds : Add Picoseconds
addPicoseconds =
    add fromPicoseconds


add : (a -> Time) -> a -> Time -> Time
add toTime a (Time psA) =
    let
        (Time psB) =
            toTime a
    in
    Time <| Picoseconds.add psA psB



-- SERIALIZATION


fromPosix : Time.Posix -> Time
fromPosix =
    Time.posixToMillis >> Z.fromSafeInt >> Milliseconds.fromInteger >> fromMilliseconds


{-| Converts a `Time` into a `Time.Posix` from `elm/time`.

This conversion is done by first converting `Time`'s internal `Integer` to a regular `Int`,
then from that `Int` to `Time.Posix` using `millisToPosix`. Since `Integer` serializes integers of any length,
there is a possibility that data can be lost in the conversion (`Int`s are limited by CPU word length).
In this case, the resulting `Time.Posix` is generated from `0` as a fallback.

-}
toPosix : Time -> Time.Posix
toPosix time =
    toMilliseconds time
        |> Milliseconds.toInteger
        |> Z.toString
        |> String.toInt
        |> Maybe.withDefault 0
        |> Time.millisToPosix


toWeeks : Time -> Weeks
toWeeks time =
    toDays time
        |> Days.toInteger
        |> divBy 7
        |> Weeks.fromInteger


fromWeeks : Weeks -> Time
fromWeeks weeks =
    Weeks.toInteger weeks
        |> mulBy 7
        |> Days.fromInteger
        |> fromDays


toDays : Time -> Days
toDays time =
    toHours time
        |> Hours.toInteger
        |> divBy 24
        |> Days.fromInteger


fromDays : Days -> Time
fromDays days =
    Days.toInteger days
        |> mulBy 24
        |> Hours.fromInteger
        |> fromHours


toHours : Time -> Hours
toHours time =
    toMinutes time
        |> Minutes.toInteger
        |> divBy 60
        |> Hours.fromInteger


fromHours : Hours -> Time
fromHours hours =
    Hours.toInteger hours
        |> mulBy 60
        |> Minutes.fromInteger
        |> fromMinutes


toMinutes : Time -> Minutes
toMinutes time =
    toSeconds time
        |> Seconds.toInteger
        |> divBy 60
        |> Minutes.fromInteger


fromMinutes : Minutes -> Time
fromMinutes minutes =
    Minutes.toInteger minutes
        |> mulBy 60
        |> Seconds.fromInteger
        |> fromSeconds


toSeconds : Time -> Seconds
toSeconds (Time ps) =
    Z.safePow10 12
        |> Z.safeDiv (Picoseconds.toInteger ps)
        |> Seconds.fromInteger


fromSeconds : Seconds -> Time
fromSeconds seconds =
    Z.safePow10 12
        |> Z.mul (Seconds.toInteger seconds)
        |> Picoseconds.fromInteger
        |> Time


toMilliseconds : Time -> Milliseconds
toMilliseconds (Time ps) =
    Z.safePow10 9
        |> Z.safeDiv (Picoseconds.toInteger ps)
        |> Milliseconds.fromInteger


fromMilliseconds : Milliseconds -> Time
fromMilliseconds milliseconds =
    Z.safePow10 9
        |> Z.mul (Milliseconds.toInteger milliseconds)
        |> Picoseconds.fromInteger
        |> Time


toMicroseconds : Time -> Microseconds
toMicroseconds (Time ps) =
    Z.safePow10 6
        |> Z.safeDiv (Picoseconds.toInteger ps)
        |> Microseconds.fromInteger


fromMicroseconds : Microseconds -> Time
fromMicroseconds microseconds =
    Z.safePow10 6
        |> Z.mul (Microseconds.toInteger microseconds)
        |> Picoseconds.fromInteger
        |> Time


toNanoseconds : Time -> Nanoseconds
toNanoseconds (Time ps) =
    Z.safePow10 3
        |> Z.safeDiv (Picoseconds.toInteger ps)
        |> Nanoseconds.fromInteger


fromNanoseconds : Nanoseconds -> Time
fromNanoseconds nanoseconds =
    Z.safePow10 3
        |> Z.mul (Nanoseconds.toInteger nanoseconds)
        |> Picoseconds.fromInteger
        |> Time


toPicoseconds : Time -> Picoseconds
toPicoseconds (Time ps) =
    ps


fromPicoseconds : Picoseconds -> Time
fromPicoseconds =
    Time



-- COMPATIBILITY HELPERS


millisToPosix =
    Time.millisToPosix


posixToMillis =
    Time.posixToMillis



-- INTERNAL HELPERS


mulBy : Int -> Integer -> Integer
mulBy n =
    Z.mul (Z.fromSafeInt n)


divBy : Int -> Integer -> Integer
divBy n =
    Z.safeDivBy (Z.fromSafeInt n)
