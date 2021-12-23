module Lib.Time.Date exposing
    ( Add
    , Date
    , addDays
    , addWeeks
    , date
    , fromCivil
    , fromTime
    , getDayOfMonth
    , getMonth
    , getWeekday
    , getYear
    , toCivil
    , toTime
    , today
    )

import BigInt exposing (BigInt)
import Lib.Time as Time exposing (Time)
import Lib.Time.Civil as Civil exposing (Civil)
import Lib.Time.Days as Days exposing (Days)
import Lib.Time.Month as Month exposing (Month)
import Lib.Time.Weekday exposing (Weekday)
import Lib.Time.Weeks exposing (Weeks)
import Lib.Time.Years as Years exposing (Years)
import Task exposing (Task)



-- TYPES


type Date
    = Date Years Month Days



-- GENERAL HELPERS


date : Years -> Month -> Days -> Date
date =
    Date


toTime : Time.Zone -> Date -> Time
toTime zone date_ =
    toCivil zone date_
        |> Civil.toTime


fromTime : Time.Zone -> Time -> Date
fromTime zone time =
    let
        years =
            Time.getYear zone time

        month =
            Time.getMonth zone time

        days =
            Time.getDayOfMonth zone time
    in
    date years month days


toCivil : Time.Zone -> Date -> Civil
toCivil zone (Date year month day) =
    Civil.date zone year month day


fromCivil : Civil -> Date
fromCivil civil =
    Civil.toTime civil
        |> fromTime (Civil.getZone civil)



-- TASKS


today : Time.Zone -> Task x Date
today zone =
    Task.map (fromTime zone) Time.now



-- HUMAN-READABLE


getYear : Date -> Years
getYear (Date y _ _) =
    y


getMonth : Date -> Month
getMonth (Date _ m _) =
    m


getWeekday : Date -> Weekday
getWeekday =
    toTime Time.utc >> Time.getWeekday Time.utc


getDayOfMonth : Date -> Days
getDayOfMonth (Date _ _ d) =
    d



-- ARITHMETIC


type alias Add units =
    units -> Date -> Date


addWeeks : Add Weeks
addWeeks =
    add Time.addWeeks


addDays : Add Days
addDays =
    add Time.addDays


add : Time.Add a -> a -> Date -> Date
add addUnit amount date_ =
    toTime Time.utc date_
        |> addUnit amount
        |> fromTime Time.utc
