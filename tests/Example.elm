module Example exposing (..)

import Data
import Expect
import List.Extra
import Main exposing (isNextSentence, nextNewSentenceType, organizeAcceptabilityTrials, removesItems)
import Pretest.Acceptability as A
import Session2.Spelling exposing (dedupe)
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
            , test "Turns a list into n sublists of 1 elements" <|
                \_ ->
                    Expect.equal [ [ 1 ], [ 2 ] ] (Data.splitIn 1 [ 1, 2 ])
            , test "Turns a list into 1 sublist of 2 elements" <|
                \_ ->
                    Expect.equal [ [ 1, 2 ] ] (Data.splitIn 2 [ 1, 2 ])
            ]
        , describe "RemovesItems removes given items from a list"
            [ test "an empty ls without items to remove" <|
                \_ ->
                    Expect.equal [] (removesItems [] [])
            , test "The same list is returned when no items to be removed are specified." <|
                \_ ->
                    Expect.equal [ "hey" ] (removesItems [] [ "hey" ])
            , test "The same list with more than 1 elements is returned when no items to be removed are specified" <|
                \_ ->
                    Expect.equal [ "hey", "yo" ] (removesItems [] [ "hey", "yo" ])
            , test "Items are properly removed when specified" <|
                \_ ->
                    Expect.equal [ "hey" ] (removesItems [ "yo" ] [ "hey", "yo" ])
            , test "All items are removed" <|
                \_ ->
                    Expect.equal [] (removesItems [ "hey", "yo" ] [ "hey", "yo" ])
            , test "Order of items to be remove doesn't matter" <|
                \_ ->
                    Expect.equal [] (removesItems [ "yo", "hey" ] [ "hey", "yo" ])
            , test "When list does not contain the item to be removed, returns list unchanged" <|
                \_ ->
                    Expect.equal [ "hey", "yo" ] (removesItems [ "popey" ] [ "hey", "yo" ])
            ]
        , describe "isNextSentence test suite"
            [ test "Sentence is a valid next sentence" <|
                \_ ->
                    Expect.equal True (isNextSentence { sentenceType = A.RelativeClause } [])
            , test "We already have this type of sentence thus this is not a valid sentence" <|
                \_ ->
                    Expect.equal False (isNextSentence { sentenceType = A.RelativeClause } [ { sentenceType = A.RelativeClause } ])
            , test "Sentence is a valid next sentence because there is no such type already" <|
                \_ ->
                    Expect.equal True (isNextSentence { sentenceType = A.RelativeClause } [ { sentenceType = A.EmbeddedQuestion } ])
            , test "Sentence is a unvalid" <|
                \_ ->
                    Expect.equal False (isNextSentence { sentenceType = A.RelativeClause } [ { sentenceType = A.EmbeddedQuestion }, { sentenceType = A.RelativeClause } ])
            ]
        , describe "Find next valid sentence"
            [ test "Returns Nothing because list to search is empty" <|
                \_ ->
                    Expect.equal Nothing (List.Extra.find (isNextSentence { sentenceType = A.RelativeClause }) [])
            , test "Returns the first element of the list when buffer is empty" <|
                \_ ->
                    let
                        searchedList =
                            [ { sentenceType = A.RelativeClause } ]

                        predicate =
                            nextNewSentenceType []

                        expectedResult =
                            Just { sentenceType = A.RelativeClause }
                    in
                    Expect.equal expectedResult (List.Extra.find predicate searchedList)
            , test "Returns nothing when the searchedList is empty" <|
                \_ ->
                    let
                        searchedList =
                            []

                        predicate =
                            nextNewSentenceType []

                        expectedResult =
                            Nothing
                    in
                    Expect.equal expectedResult (List.Extra.find predicate searchedList)
            , test "Returns nothing when the next item category is already in buff" <|
                \_ ->
                    let
                        searchedList =
                            [ { sentenceType = A.RelativeClause } ]

                        predicate =
                            nextNewSentenceType [ { sentenceType = A.RelativeClause } ]

                        expectedResult =
                            Nothing
                    in
                    Expect.equal expectedResult (List.Extra.find predicate searchedList)
            ]
        ]


testTarget =
    { uid = "uid"
    , sentence = "sentence"
    , sentenceType = A.EmbeddedQuestion
    , trialType = A.Target
    , isGrammatical = True
    , audio = Data.AudioFile "" ""
    }


testDistractor =
    { uid = "uid"
    , sentence = "sentence"
    , sentenceType = A.RelativeClause
    , trialType = A.Distractor
    , isGrammatical = True
    , audio = Data.AudioFile "" ""
    }
