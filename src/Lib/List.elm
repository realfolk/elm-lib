module Lib.List exposing (groupBy)

{-| Group a `List` into sublists of size `n`. The last list may have less than `n` elements.
-}


groupBy : Int -> List a -> List (List a)
groupBy n list =
    case list of
        [] ->
            []

        _ ->
            List.take n list :: groupBy n (List.drop n list)
