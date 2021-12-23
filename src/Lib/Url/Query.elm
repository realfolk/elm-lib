module Lib.Url.Query exposing
    ( Query
    , QueryItem
    , fromList
    , fromString
    , toList
    , toString
    )

import Dict exposing (Dict)



-- TYPES


type Query
    = Query (Dict String String)


type alias QueryItem =
    ( String, String )



-- CONSTRUCT


fromString : String -> Query
fromString query =
    let
        toQueryItem rawItem =
            case rawItem of
                [ "" ] ->
                    Nothing

                [ name ] ->
                    Just ( name, "" )

                [ name, "" ] ->
                    Just ( name, "" )

                [ name, value ] ->
                    Just ( name, value )

                _ ->
                    Nothing
    in
    query
        |> String.split "&"
        |> List.filterMap (String.split "=" >> List.filter ((/=) "") >> toQueryItem)
        |> fromList


fromList : List QueryItem -> Query
fromList =
    Query << Dict.fromList



-- CONVERT


toString : Query -> String
toString (Query query) =
    let
        f name value acc =
            name ++ equals value ++ ampersand acc

        equals value =
            if value == "" then
                ""

            else
                String.cons '=' value

        ampersand acc =
            if acc == "" then
                ""

            else
                String.cons '&' acc
    in
    Dict.foldr f "" query


toList : Query -> List QueryItem
toList (Query query) =
    Dict.toList query
