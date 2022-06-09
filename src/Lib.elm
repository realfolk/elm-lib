module Lib exposing (..)


flip : (a -> b -> c) -> b -> a -> c
flip f b a =
    f a b


isJust : Maybe a -> Bool
isJust m =
    Maybe.withDefault False <| Maybe.map (always True) m


invertOrder : Order -> Order
invertOrder o =
    case o of
        LT ->
            GT

        EQ ->
            EQ

        GT ->
            LT
