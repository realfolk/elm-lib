module Lib.Queue exposing
    ( Queue
    , dequeue
    , empty
    , enqueue
    , fromList
    , isEmpty
    , toList
    )


type Queue a
    = Queue (List a) (List a)


empty : Queue a
empty =
    Queue [] []


isEmpty : Queue a -> Bool
isEmpty (Queue front back) =
    List.isEmpty front && List.isEmpty back


fromList : List a -> Queue a
fromList list =
    Queue list []


enqueue : a -> Queue a -> Queue a
enqueue x (Queue front back) =
    Queue front (x :: back)


dequeue : Queue a -> ( Maybe a, Queue a )
dequeue (Queue front back) =
    case front of
        [] ->
            case back of
                [] ->
                    ( Nothing, empty )

                _ ->
                    dequeue (Queue (List.reverse back) [])

        h :: t ->
            ( Just h, Queue t back )


toList : Queue a -> List a
toList (Queue front back) =
    front ++ List.reverse back
