module Lib exposing (..)


isJust : Maybe a -> Bool
isJust m =
    Maybe.withDefault False <| Maybe.map (always True) m
