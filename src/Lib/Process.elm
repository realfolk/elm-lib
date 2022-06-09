module Lib.Process exposing (delay)

import Process
import Task


delay : Float -> msg -> Cmd msg
delay ms msg =
    Process.sleep ms
        |> Task.perform (always msg)
