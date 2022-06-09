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

import BigInt exposing (BigInt)
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
        |> BigInt.fromInt
        |> Years.fromBigInt


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
        |> BigInt.fromInt
        |> Days.fromBigInt


getHour : Zone -> Time -> Hours
getHour (Zone zone) time =
    toPosix time
        |> Time.toHour zone
        |> BigInt.fromInt
        |> Hours.fromBigInt


getMinute : Zone -> Time -> Minutes
getMinute (Zone zone) time =
    toPosix time
        |> Time.toMinute zone
        |> BigInt.fromInt
        |> Minutes.fromBigInt


getSecond : Zone -> Time -> Seconds
getSecond (Zone zone) time =
    toPosix time
        |> Time.toSecond zone
        |> BigInt.fromInt
        |> Seconds.fromBigInt


getMillisecond : Zone -> Time -> Milliseconds
getMillisecond (Zone zone) time =
    toPosix time
        |> Time.toMillis zone
        |> BigInt.fromInt
        |> Milliseconds.fromBigInt



-- COMPARISON


compare : Time -> Time -> Order
compare a b =
    let
        aBigInt =
            toPicoseconds a |> Picoseconds.toBigInt

        bBigInt =
            toPicoseconds b |> Picoseconds.toBigInt
    in
    BigInt.compare aBigInt bBigInt



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
    Time.posixToMillis >> BigInt.fromInt >> Milliseconds.fromBigInt >> fromMilliseconds


{-| Converts a `Time` into a `Time.Posix` from `elm/time`. This conversion is done by first converting `Time`'s internal `BigInt` to a regular `Int`, then from that `Int` to `Time.Posix` using `millisToPosix`. Since `BigInt` serializes integers of any length, there is a possibility that data can be lost in the conversion (`Int`s are limited by CPU word length). In this case, the resulting `Time.Posix` is generated from `0` as a fallback.
-}
toPosix : Time -> Time.Posix
toPosix time =
    toMilliseconds time
        |> Milliseconds.toBigInt
        |> BigInt.toString
        |> String.toInt
        |> Maybe.withDefault 0
        |> Time.millisToPosix


toWeeks : Time -> Weeks
toWeeks time =
    toDays time
        |> Days.toBigInt
        |> divBy 7
        |> Weeks.fromBigInt


fromWeeks : Weeks -> Time
fromWeeks weeks =
    Weeks.toBigInt weeks
        |> mulBy 7
        |> Days.fromBigInt
        |> fromDays


toDays : Time -> Days
toDays time =
    toHours time
        |> Hours.toBigInt
        |> divBy 24
        |> Days.fromBigInt


fromDays : Days -> Time
fromDays days =
    Days.toBigInt days
        |> mulBy 24
        |> Hours.fromBigInt
        |> fromHours


toHours : Time -> Hours
toHours time =
    toMinutes time
        |> Minutes.toBigInt
        |> divBy 60
        |> Hours.fromBigInt


fromHours : Hours -> Time
fromHours hours =
    Hours.toBigInt hours
        |> mulBy 60
        |> Minutes.fromBigInt
        |> fromMinutes


toMinutes : Time -> Minutes
toMinutes time =
    toSeconds time
        |> Seconds.toBigInt
        |> divBy 60
        |> Minutes.fromBigInt


fromMinutes : Minutes -> Time
fromMinutes minutes =
    Minutes.toBigInt minutes
        |> mulBy 60
        |> Seconds.fromBigInt
        |> fromSeconds


toSeconds : Time -> Seconds
toSeconds (Time ps) =
    pow10 12
        |> BigInt.div (Picoseconds.toBigInt ps)
        |> Seconds.fromBigInt


fromSeconds : Seconds -> Time
fromSeconds seconds =
    pow10 12
        |> BigInt.mul (Seconds.toBigInt seconds)
        |> Picoseconds.fromBigInt
        |> Time


toMilliseconds : Time -> Milliseconds
toMilliseconds (Time ps) =
    pow10 9
        |> BigInt.div (Picoseconds.toBigInt ps)
        |> Milliseconds.fromBigInt


fromMilliseconds : Milliseconds -> Time
fromMilliseconds milliseconds =
    pow10 9
        |> BigInt.mul (Milliseconds.toBigInt milliseconds)
        |> Picoseconds.fromBigInt
        |> Time


toMicroseconds : Time -> Microseconds
toMicroseconds (Time ps) =
    pow10 6
        |> BigInt.div (Picoseconds.toBigInt ps)
        |> Microseconds.fromBigInt


fromMicroseconds : Microseconds -> Time
fromMicroseconds microseconds =
    pow10 6
        |> BigInt.mul (Microseconds.toBigInt microseconds)
        |> Picoseconds.fromBigInt
        |> Time


toNanoseconds : Time -> Nanoseconds
toNanoseconds (Time ps) =
    pow10 3
        |> BigInt.div (Picoseconds.toBigInt ps)
        |> Nanoseconds.fromBigInt


fromNanoseconds : Nanoseconds -> Time
fromNanoseconds nanoseconds =
    pow10 3
        |> BigInt.mul (Nanoseconds.toBigInt nanoseconds)
        |> Picoseconds.fromBigInt
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


pow10 : Int -> BigInt
pow10 exp =
    BigInt.pow (BigInt.fromInt 10) (BigInt.fromInt exp)


mulBy : Int -> BigInt -> BigInt
mulBy n =
    BigInt.mul (BigInt.fromInt n)


divBy : Int -> BigInt -> BigInt
divBy n bi =
    BigInt.div bi (BigInt.fromInt n)
