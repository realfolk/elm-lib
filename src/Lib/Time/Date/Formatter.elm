module Lib.Time.Date.Formatter exposing
    ( DateResolution(..)
    , Formatter
    , append
    , concat
    , dayOfMonth
    , empty
    , format
    , intercalate
    , iso8601Date
    , month
    , monthNumber
    , padLeft
    , shortMonth
    , shortWeekday
    , static
    , weekday
    , weekdayNumber
    , year
    )

import Integer as Z exposing (Integer)
import Lib.Time.Date as Date exposing (Date)
import Lib.Time.Days as Days
import Lib.Time.Month as Month
import Lib.Time.Weekday as Weekday
import Lib.Time.Years as Years



-- FORMATTER


type Formatter
    = Formatter (Date -> String)


format : Formatter -> Date -> String
format (Formatter f) date =
    f date



-- BASIC FORMATTERS


empty : Formatter
empty =
    Formatter <| \_ -> ""


static : String -> Formatter
static s =
    Formatter <| \_ -> s



-- ISO-8601 FORMATTERS


type DateResolution
    = Month
    | Day


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



-- INDIVIDUAL UNIT FORMATTERS


year : Formatter
year =
    bigIntFormatter Years.toInteger Date.getYear


month : Formatter
month =
    accessorFormatter Month.toString Date.getMonth


shortMonth : Formatter
shortMonth =
    accessorFormatter Month.toShortString Date.getMonth


monthNumber : Formatter
monthNumber =
    bigIntFormatter Month.toInteger Date.getMonth


weekday : Formatter
weekday =
    accessorFormatter Weekday.toString Date.getWeekday


shortWeekday : Formatter
shortWeekday =
    accessorFormatter Weekday.toShortString Date.getWeekday


weekdayNumber : Formatter
weekdayNumber =
    bigIntFormatter Weekday.toInteger Date.getWeekday


dayOfMonth : Formatter
dayOfMonth =
    bigIntFormatter Days.toInteger Date.getDayOfMonth



-- COMBINATORS


append : Formatter -> Formatter -> Formatter
append (Formatter a) (Formatter b) =
    Formatter <|
        \date ->
            String.concat
                [ a date
                , b date
                ]


concat : List Formatter -> Formatter
concat =
    List.foldr append empty


intercalate : Formatter -> List Formatter -> Formatter
intercalate separator =
    List.intersperse separator >> concat


padLeft : Int -> Char -> Formatter -> Formatter
padLeft n char (Formatter f) =
    Formatter <| \date -> f date |> String.padLeft n char



-- INTERNAL HELPERS


accessorFormatter : (a -> String) -> (Date -> a) -> Formatter
accessorFormatter toString accessor =
    Formatter <| accessor >> toString


bigIntFormatter : (a -> Integer) -> (Date -> a) -> Formatter
bigIntFormatter toInteger accessor =
    let
        toString =
            toInteger >> Z.toString
    in
    accessorFormatter toString accessor
