module Lib.Stack exposing
    ( Stack
    , empty
    , fromList
    , isEmpty
    , pop
    , push
    , size
    , top
    )


type Stack a
    = Empty
    | NonEmpty a (Stack a)


empty : Stack a
empty =
    Empty


isEmpty : Stack a -> Bool
isEmpty stack =
    case stack of
        Empty ->
            True

        _ ->
            False


fromList : List a -> Stack a
fromList list =
    case list of
        [] ->
            Empty

        h :: t ->
            NonEmpty h (fromList t)


push : a -> Stack a -> Stack a
push =
    NonEmpty


top : Stack a -> Maybe a
top =
    pop >> Tuple.first


pop : Stack a -> ( Maybe a, Stack a )
pop stack =
    case stack of
        Empty ->
            ( Nothing, stack )

        NonEmpty x rest ->
            ( Just x, rest )


size : Stack a -> Int
size stack =
    case stack of
        Empty ->
            0

        NonEmpty _ rest ->
            1 + size rest
