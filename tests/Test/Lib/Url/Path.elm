module Test.Lib.Url.Path exposing (suite)

import Expect
import Lib.Url.Path as Path
import Test exposing (..)


cases =
    [ { fromString = "/foo/bar/baz"
      , toString = "/foo/bar/baz"
      , path = [ "foo", "bar", "baz" ]
      }
    , { fromString = "///"
      , toString = "///"
      , path = [ "", "", "" ]
      }
    , { fromString = ""
      , toString = "/"
      , path = [ "" ]
      }
    , { fromString = "/"
      , toString = "/"
      , path = [ "" ]
      }
    ]


suite : Test
suite =
    describe "Lib.Url.Path"
        [ fromString
        , toString
        ]


fromString : Test
fromString =
    let
        makeTest i case_ =
            test (String.fromInt i ++ " \"" ++ case_.fromString ++ "\"") <|
                \_ ->
                    Path.fromString case_.fromString
                        |> Expect.equal case_.path
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
                    Path.toString case_.path
                        |> Expect.equal case_.toString
    in
    cases
        |> List.indexedMap makeTest
        |> describe "toString"
