module Lib.RemoteData exposing (RemoteData(..), map, withDefault)


type RemoteData e a
    = Loading
    | Failure e
    | Success a


withDefault : a -> RemoteData e a -> a
withDefault default d =
    case d of
        Loading ->
            default

        Failure _ ->
            default

        Success a ->
            a


map : (a -> b) -> RemoteData e a -> RemoteData e b
map f d =
    case d of
        Loading ->
            Loading

        Failure e ->
            Failure e

        Success a ->
            Success (f a)
