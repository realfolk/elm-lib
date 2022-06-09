module Lib.Basics exposing (invertOrder)


invertOrder : Order -> Order
invertOrder o =
    case o of
        LT ->
            GT

        EQ ->
            EQ

        GT ->
            LT
