module Test.Lib.Time.Civil exposing (suite)

import BigInt
import Expect
import Lib.Time as Time exposing (Time)
import Lib.Time.Civil as Civil
import Lib.Time.Days as Days
import Lib.Time.Formatter as TimeFmt
import Lib.Time.Hours as Hours
import Lib.Time.Milliseconds as Milliseconds
import Lib.Time.Minutes as Minutes
import Lib.Time.Month as Month
import Lib.Time.Seconds as Seconds
import Lib.Time.Years as Years
import Test exposing (..)


cases =
    { times =
        [ { description = "Randomly chosen date in UTC"
          , civil =
                Civil.dateAndTime
                    Time.utc
                    (Years.fromInt 2000)
                    Month.April
                    (Days.fromInt 28)
                    (Hours.fromInt 17)
                    (Minutes.fromInt 45)
                    (Seconds.fromInt 20)
                    (Milliseconds.fromInt 5)
          , time = Time.fromMilliseconds <| Milliseconds.fromInt 956943920005
          }
        , { description = "Randomly chosen date in GMT-8"
          , civil =
                Civil.dateAndTime
                    (Time.customZone (-8 * 60) [])
                    (Years.fromInt 2021)
                    Month.December
                    (Days.fromInt 1)
                    (Hours.fromInt 4)
                    (Minutes.fromInt 0)
                    (Seconds.fromInt 0)
                    (Milliseconds.fromInt 0)
          , time = Time.fromMilliseconds <| Milliseconds.fromInt 1638360000000
          }
        , { description = "Start of the epoch in UTC"
          , civil =
                Civil.dateAndTime
                    Time.utc
                    (Years.fromInt 1970)
                    Month.January
                    (Days.fromInt 1)
                    (Hours.fromInt 0)
                    (Minutes.fromInt 0)
                    (Seconds.fromInt 0)
                    (Milliseconds.fromInt 0)
          , time = Time.fromMilliseconds <| Milliseconds.fromInt 0
          }
        , { description = "Start of the epoch in UTC+10"
          , civil =
                Civil.dateAndTime
                    (Time.customZone (10 * 60) [])
                    (Years.fromInt 1970)
                    Month.January
                    (Days.fromInt 1)
                    (Hours.fromInt 0)
                    (Minutes.fromInt 0)
                    (Seconds.fromInt 0)
                    (Milliseconds.fromInt 0)
          , time = Time.fromMilliseconds <| Milliseconds.fromInt <| -10 * 60 * 60 * 1000
          }
        ]
    , zones =
        [ 0
        , -8 * 60 -- Vancouver
        , 10 * 60 -- Queensland
        , -4 * 60 -- Trinidad and Tobago
        , 1 * 60 -- Italy
        ]
    }


suite : Test
suite =
    describe "Elemental.Lib.Time.Civil"
        [ toTime
        , getZoneOffset
        ]


toTime : Test
toTime =
    let
        toString =
            Time.toMilliseconds >> Milliseconds.toBigInt >> BigInt.toString

        makeTest time =
            test (time.description ++ " (" ++ formatTime time.time ++ ")") <|
                \_ ->
                    Civil.toTime time.civil
                        |> toString
                        |> Expect.equal (toString time.time)
    in
    describe "toTime" <|
        List.map makeTest cases.times


getZoneOffset : Test
getZoneOffset =
    let
        offsetToString offset =
            let
                prefix =
                    if offset > 0 then
                        "+"

                    else
                        ""

                hours =
                    offset
                        // 60
                        |> String.fromInt
                        |> String.padLeft 2 '0'

                minutes =
                    modBy 60 offset
                        |> String.fromInt
                        |> String.padLeft 2 '0'
            in
            "UTC" ++ prefix ++ hours ++ ":" ++ minutes

        makeTest offset =
            test (offsetToString offset) <|
                \_ ->
                    Civil.getZoneOffset (Time.customZone offset [])
                        |> Milliseconds.toBigInt
                        |> BigInt.toString
                        |> Expect.equal (String.fromInt <| offset * 60 * 1000)
    in
    describe "getZoneOffset" <|
        List.map makeTest cases.zones



-- HELPERS


formatTime : Time -> String
formatTime =
    TimeFmt.format (TimeFmt.iso8601DateAndTime TimeFmt.Day TimeFmt.Millisecond) Time.utc
