module Lib.Maybe exposing
    ( foldlToMaybe
    , foldrToMaybe
    , mapToMaybe
    , sequence
    , toBool
    )

{-| Convert a `Maybe` to a `Bool`. A `Just` results in `True`, and `Nothing` results in `False`.
-}


toBool : Maybe a -> Bool
toBool =
    Maybe.map (always True) >> Maybe.withDefault False


{-| Right-fold a `List` into a `Maybe`. Returns `Just` if the supplied function always succeeds by accumulating a `Just`. Returns `Nothing` if the supplied function ever returns `Nothing`. The supplied function is not called with subsequent list items after it returns `Nothing`, effectively short-circuiting evaluation.
-}
foldrToMaybe : (a -> b -> Maybe b) -> b -> List a -> Maybe b
foldrToMaybe f b as_ =
    List.foldr (accumulateMaybe f) (Just b) as_


{-| Similar to `foldrToMaybe`, except using a left-fold.
-}
foldlToMaybe : (a -> b -> Maybe b) -> b -> List a -> Maybe b
foldlToMaybe f b as_ =
    List.foldl (accumulateMaybe f) (Just b) as_


{-| Map over the items in a `List` into a `Maybe`. Returns the mapped list in a `Just` if the supplied function always succeeds by returning a `Just` for each item in the `List`. Returns `Nothing` if the supplied function ever returns `Nothing`. The supplied function is not called with subsequent list items after it returns `Nothing`, effectively short-circuiting evaluation.
-}
mapToMaybe : (a -> Maybe b) -> List a -> Maybe (List b)
mapToMaybe f as_ =
    let
        g a bs =
            f a |> Maybe.map (\b -> b :: bs)
    in
    foldrToMaybe g [] as_


{-| Helper function to chain computations that return a `Maybe`.
-}
accumulateMaybe : (a -> b -> Maybe b) -> a -> Maybe b -> Maybe b
accumulateMaybe f a maybeB =
    Maybe.andThen (f a) maybeB


{-| Convert a `List (Maybe a)` to a `Maybe (List a)`. If any of the list's items are `Nothing`, the return value will be `Nothing`
-}
sequence : List (Maybe a) -> Maybe (List a)
sequence =
    let
        f m acc =
            Maybe.map2 (::) m acc
    in
    List.foldr f (Just [])
