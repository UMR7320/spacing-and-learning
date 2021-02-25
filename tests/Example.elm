module Example exposing (..)

import Data
import Expect exposing (Expectation)
import Fuzz exposing (Fuzzer, int, list, string)
import Session2.Scrabble exposing (dedupe)
import Test exposing (..)


suite : Test
suite =
    describe "My test suite"
        [ describe "The dedupe function"
            [ test "doubled letters are deduplicated" <|
                \_ ->
                    Expect.equal [ ( "a", 1 ), ( "a", 2 ), ( "b", 1 ) ] (dedupe [ "a", "a", "b" ])
            , test "Given empty list return empty list" <|
                \_ ->
                    Expect.equal [] (dedupe [])
            ]
        , describe "The splitIn function"
            [ test "An empty list returns an empty list" <|
                \_ ->
                    Expect.equal [ [] ] (Data.splitIn 1 [])
            , test "Turns a list into a sublist of itself" <|
                \_ ->
                    Expect.equal [ [ 1 ], [ 2 ] ] (Data.splitIn 1 [ 1, 2 ])
            ]
        ]
