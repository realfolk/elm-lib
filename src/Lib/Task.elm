module Lib.Task exposing (dispatch, safely)

import Task


safely : msg -> (a -> msg) -> Task.Task x a -> Cmd msg
safely fallback toMsg task =
    Task.attempt (Result.map toMsg >> Result.withDefault fallback) task


dispatch : msg -> Cmd msg
dispatch msg =
    Task.succeed msg
        |> Task.perform (always msg)
