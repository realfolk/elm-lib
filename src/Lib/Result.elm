module Lib.Result exposing
    ( foldlToResult
    , foldrToResult
    , mapToResult
    )

{-| Right-fold a `List` into a `Result`. Returns `Ok` if the supplied function always succeeds by accumulating a `Ok`. Returns `Err` if the supplied function ever returns `Err`. The supplied function is not called with subsequent list items after it returns `Err`, effectively short-circuiting evaluation.
-}


foldrToResult : (a -> b -> Result e b) -> b -> List a -> Result e b
foldrToResult f b as_ =
    List.foldr (accumulateResult f) (Ok b) as_


{-| Similar to `foldrToResult`, except using a left-fold.
-}
foldlToResult : (a -> b -> Result e b) -> b -> List a -> Result e b
foldlToResult f b as_ =
    List.foldl (accumulateResult f) (Ok b) as_


{-| Map over the items in a `List` into a `Result`. Returns the mapped list in a `Ok` if the supplied function always succeeds by returning a `Ok` for each item in the `List`. Returns `Err` if the supplied function ever returns `Err`. The supplied function is not called with subsequent list items after it returns `Err`, effectively short-circuiting evaluation.
-}
mapToResult : (a -> Result e b) -> List a -> Result e (List b)
mapToResult f as_ =
    let
        g a bs =
            f a |> Result.map (\b -> b :: bs)
    in
    foldrToResult g [] as_


{-| Helper function to chain computations that return a `Result`.
-}
accumulateResult : (a -> b -> Result e b) -> a -> Result e b -> Result e b
accumulateResult f a maybeB =
    Result.andThen (f a) maybeB
