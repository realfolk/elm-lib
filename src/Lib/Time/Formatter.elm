module Lib.Time.Formatter exposing
    ( DateResolution(..)
    , Formatter
    , TimeResolution(..)
    , append
    , concat
    , dayOfMonth
    , empty
    , format
    , hour
    , intercalate
    , iso8601Date
    , iso8601DateAndTime
    , iso8601Time
    , millisecond
    , minute
    , month
    , monthNumber
    , padLeft
    , second
    , shortMonth
    , shortWeekday
    , static
    , weekday
    , weekdayNumber
    , year
    )

import Integer as Z exposing (Integer)
import Lib.Time as Time exposing (Time)
import Lib.Time.Days as Days
import Lib.Time.Hours as Hours
import Lib.Time.Milliseconds as Milliseconds
import Lib.Time.Minutes as Minutes
import Lib.Time.Month as Month
import Lib.Time.Seconds as Seconds
import Lib.Time.Weekday as Weekday
import Lib.Time.Years as Years



-- FORMATTER


type Formatter
    = Formatter (Time.Zone -> Time -> String)


format : Formatter -> Time.Zone -> Time -> String
format (Formatter f) zone time =
    f zone time



-- BASIC FORMATTERS


empty : Formatter
empty =
    Formatter <| \_ _ -> ""


static : String -> Formatter
static s =
    Formatter <| \_ _ -> s



-- ISO-8601 FORMATTERS


type DateResolution
    = Month
    | Day


type TimeResolution
    = Hour
    | Minute
    | Second
    | Millisecond


iso8601DateAndTime : DateResolution -> TimeResolution -> Formatter
iso8601DateAndTime dateResolution timeResolution =
    concat
        [ iso8601Date dateResolution
        , static "T"
        , iso8601Time timeResolution
        ]


iso8601Date : DateResolution -> Formatter
iso8601Date resolution =
    let
        separator =
            static "-"

        year_ =
            padLeft 4 '0' year

        month_ =
            padLeft 2 '0' monthNumber

        day_ =
            padLeft 2 '0' dayOfMonth
    in
    concat <|
        List.intersperse separator <|
            case resolution of
                Month ->
                    [ year_, month_ ]

                Day ->
                    [ year_, month_, day_ ]


iso8601Time : TimeResolution -> Formatter
iso8601Time resolution =
    let
        separator =
            static ":"

        hour_ =
            padLeft 2 '0' hour

        minute_ =
            padLeft 2 '0' minute

        second_ =
            padLeft 2 '0' second

        millisecond_ =
            padLeft 3 '0' millisecond
    in
    concat <|
        List.intersperse separator <|
            case resolution of
                Hour ->
                    [ hour_ ]

                Minute ->
                    [ hour_, minute_ ]

                Second ->
                    [ hour_, minute_, second_ ]

                Millisecond ->
                    [ hour_
                    , minute_
                    , concat
                        [ second_
                        , static "."
                        , millisecond_
                        ]
                    ]



-- INDIVIDUAL UNIT FORMATTERS


year : Formatter
year =
    bigIntFormatter Years.toInteger Time.getYear


month : Formatter
month =
    accessorFormatter Month.toString Time.getMonth


shortMonth : Formatter
shortMonth =
    accessorFormatter Month.toShortString Time.getMonth


monthNumber : Formatter
monthNumber =
    bigIntFormatter Month.toInteger Time.getMonth


weekday : Formatter
weekday =
    accessorFormatter Weekday.toString Time.getWeekday


shortWeekday : Formatter
shortWeekday =
    accessorFormatter Weekday.toShortString Time.getWeekday


weekdayNumber : Formatter
weekdayNumber =
    bigIntFormatter Weekday.toInteger Time.getWeekday


dayOfMonth : Formatter
dayOfMonth =
    bigIntFormatter Days.toInteger Time.getDayOfMonth


hour : Formatter
hour =
    bigIntFormatter Hours.toInteger Time.getHour


minute : Formatter
minute =
    bigIntFormatter Minutes.toInteger Time.getMinute


second : Formatter
second =
    bigIntFormatter Seconds.toInteger Time.getSecond


millisecond : Formatter
millisecond =
    bigIntFormatter Milliseconds.toInteger Time.getMillisecond



-- COMBINATORS


append : Formatter -> Formatter -> Formatter
append (Formatter a) (Formatter b) =
    Formatter <|
        \zone time ->
            String.concat
                [ a zone time
                , b zone time
                ]


concat : List Formatter -> Formatter
concat =
    List.foldr append empty


intercalate : Formatter -> List Formatter -> Formatter
intercalate separator =
    List.intersperse separator >> concat


padLeft : Int -> Char -> Formatter -> Formatter
padLeft n char (Formatter f) =
    Formatter <| \zone time -> f zone time |> String.padLeft n char



-- INTERNAL HELPERS


accessorFormatter : (a -> String) -> (Time.Zone -> Time -> a) -> Formatter
accessorFormatter toString accessor =
    Formatter <| \zone time -> toString <| accessor zone time


bigIntFormatter : (a -> Integer) -> (Time.Zone -> Time -> a) -> Formatter
bigIntFormatter toInteger accessor =
    let
        toString =
            toInteger >> Z.toString
    in
    accessorFormatter toString accessor
