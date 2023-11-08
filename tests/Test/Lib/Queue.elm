module Test.Lib.Queue exposing (suite)

import Expect
import Lib.Queue as Queue
import Test exposing (..)


suite : Test
suite =
    describe "Lib.Queue"
        [ constructorSuite
        , enqueueSuite
        , dequeueSuite
        ]


constructorSuite : Test
constructorSuite =
    describe "constructors"
        [ test "empty" <|
            \_ ->
                Queue.empty
                    |> Queue.isEmpty
                    |> Expect.equal True
        , describe "fromList"
            [ test "[]" <|
                \_ ->
                    Queue.fromList []
                        |> Queue.isEmpty
                        |> Expect.equal True
            , test "[1]" <|
                \_ ->
                    Queue.fromList [ 1 ]
                        |> Queue.toList
                        |> Expect.equal [ 1 ]
            , test "[1, 2, 3]" <|
                \_ ->
                    Queue.fromList [ 1, 2, 3 ]
                        |> Queue.toList
                        |> Expect.equal [ 1, 2, 3 ]
            ]
        ]


enqueueSuite : Test
enqueueSuite =
    describe "enqueue"
        [ test "[1]" <|
            \_ ->
                Queue.empty
                    |> Queue.enqueue 1
                    |> Queue.toList
                    |> Expect.equal [ 1 ]
        , test "[1,2,3]" <|
            \_ ->
                Queue.empty
                    |> Queue.enqueue 1
                    |> Queue.enqueue 2
                    |> Queue.enqueue 3
                    |> Queue.toList
                    |> Expect.equal [ 1, 2, 3 ]
        ]


dequeueSuite : Test
dequeueSuite =
    describe "dequeue"
        [ test "[]" <|
            \_ ->
                Queue.empty
                    |> Queue.dequeue
                    |> Tuple.mapSecond Queue.toList
                    |> Expect.equal ( Nothing, [] )
        , test "[1]" <|
            \_ ->
                Queue.fromList [ 1 ]
                    |> Queue.dequeue
                    |> Tuple.mapSecond Queue.toList
                    |> Expect.equal ( Just 1, [] )
        , test "[1, 2, 3]" <|
            \_ ->
                Queue.fromList [ 1, 2, 3 ]
                    |> Queue.dequeue
                    |> Tuple.mapSecond Queue.toList
                    |> Expect.equal ( Just 1, [ 2, 3 ] )
        ]
