module Lib.List exposing
    ( groupBy
    , indexedFoldl
    , sortByWith
    , zip
    )

{-| Group a `List` into sublists of size `n`.

The last list may have less than `n` elements.

-}


groupBy : Int -> List a -> List (List a)
groupBy n list =
    case list of
        [] ->
            []

        _ ->
            List.take n list :: groupBy n (List.drop n list)


indexedFoldl : (( Int, a ) -> b -> b) -> b -> List a -> b
indexedFoldl step base items =
    List.foldl step base <| zip (List.range 0 (List.length items - 1)) items


{-| Sort a list using the given accessor and custom sort function.
-}
sortByWith : (a -> comparable) -> (comparable -> comparable -> Order) -> List a -> List a
sortByWith accessorFn sortFn input =
    List.sortWith (\a b -> sortFn (accessorFn a) (accessorFn b)) input


zip : List x -> List y -> List ( x, y )
zip xs ys =
    List.map2 Tuple.pair xs ys
