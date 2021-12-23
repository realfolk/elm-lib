module Lib.Url.Path exposing
    ( Path
    , fromString
    , toString
    )


type alias Path =
    List String


fromString : String -> Path
fromString path =
    let
        unprefixedPath =
            case String.uncons path of
                Just ( '/', rest ) ->
                    rest

                _ ->
                    path
    in
    String.split "/" unprefixedPath


toString : Path -> String
toString path =
    path
        |> List.intersperse "/"
        |> String.concat
        |> String.cons '/'
