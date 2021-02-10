module Example exposing (..)

import Expect exposing (Expectation)
import Experiment.Scrabble exposing (dedupe)
import Fuzz exposing (Fuzzer, int, list, string)
import Test exposing (..)


suite : Test
suite =
    describe "The dedupe function"
        [ test "doubled letters are deduplicated" <|
            \_ ->
                Expect.equal [ ( "a", 1 ), ( "a", 2 ), ( "b", 1 ) ] (dedupe [ "a", "a", "b" ])
        , test "Given empty list return empty list" <|
            \_ ->
                Expect.equal [] (dedupe [])
        ]
