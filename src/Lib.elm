module Lib exposing (..)

import Html.Styled as H
import Lib.Time as Time
import Process
import Task
import Tuple


type alias Element msg =
    List (H.Attribute msg) -> List (H.Html msg) -> H.Html msg


flip : (a -> b -> c) -> b -> a -> c
flip f b a =
    f a b


iff : Bool -> a -> a -> a
iff condition then_ else_ =
    if condition then
        then_

    else
        else_


zip : List x -> List y -> List ( x, y )
zip xs ys =
    List.map2 Tuple.pair xs ys


delay : Float -> msg -> Cmd msg
delay ms msg =
    Process.sleep ms
        |> Task.perform (always msg)


getTimeZone : (Time.Zone -> msg) -> Cmd msg
getTimeZone toMsg =
    safely (toMsg Time.utc) toMsg Time.here


safely : msg -> (a -> msg) -> Task.Task x a -> Cmd msg
safely fallback toMsg task =
    Task.attempt (Result.map toMsg >> Result.withDefault fallback) task


dispatch : msg -> Cmd msg
dispatch msg =
    safely msg identity <| Task.succeed msg


isJust : Maybe a -> Bool
isJust m =
    Maybe.withDefault False <| Maybe.map (always True) m


indexedFoldl : (( Int, a ) -> b -> b) -> b -> List a -> b
indexedFoldl step base items =
    List.foldl step base <| zip (List.range 0 (List.length items - 1)) items


{-| Sort a list using the given accessor and custom sort function.
-}
sortByWith : (a -> comparable) -> (comparable -> comparable -> Order) -> List a -> List a
sortByWith accessorFn sortFn input =
    List.sortWith (\a b -> sortFn (accessorFn a) (accessorFn b)) input


invertOrder : Order -> Order
invertOrder o =
    case o of
        LT ->
            GT

        EQ ->
            EQ

        GT ->
            LT
