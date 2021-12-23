module Test.Lib.Url.Query exposing (suite)

import Expect
import Lib.Url.Query as Query
import Test exposing (..)


cases =
    [ { fromString = "foo=bar&baz"
      , toString = "foo=bar&baz"
      , query = [ ( "foo", "bar" ), ( "baz", "" ) ]
      }
    , { fromString = ""
      , toString = ""
      , query = []
      }
    , { fromString = "foo&bar&baz"
      , toString = "foo&bar&baz"
      , query = [ ( "foo", "" ), ( "bar", "" ), ( "baz", "" ) ]
      }
    , { fromString = "&foo&&&&bar&&baz&"
      , toString = "foo&bar&baz"
      , query = [ ( "foo", "" ), ( "bar", "" ), ( "baz", "" ) ]
      }
    , { fromString = "foo=baz&bar==baz&borp====&zip=&zap"
      , toString = "foo=baz&bar=baz&borp&zip&zap"
      , query = [ ( "foo", "baz" ), ( "bar", "baz" ), ( "borp", "" ), ( "zip", "" ), ( "zap", "" ) ]
      }
    , { fromString = "&&&&"
      , toString = ""
      , query = []
      }
    ]


suite : Test
suite =
    describe "Lib.Url.Query"
        [ fromString
        , toString
        ]


fromString : Test
fromString =
    let
        makeTest i case_ =
            test (String.fromInt i ++ " \"" ++ case_.fromString ++ "\"") <|
                \_ ->
                    Query.fromString case_.fromString
                        |> Expect.equal (Query.fromList case_.query)
    in
    cases
        |> List.indexedMap makeTest
        |> describe "fromString"


toString : Test
toString =
    let
        makeTest i case_ =
            test (String.fromInt i ++ " \"" ++ case_.toString ++ "\"") <|
                \_ ->
                    Query.toString (Query.fromList case_.query)
                        |> Expect.equal (Query.toString (Query.fromString case_.toString))
    in
    cases
        |> List.indexedMap makeTest
        |> describe "toString"
