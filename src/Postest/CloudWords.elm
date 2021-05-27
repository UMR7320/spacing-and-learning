module Postest.CloudWords exposing (WordKnowledge, toggle, words)

import Dict


type WordKnowledge
    = Known
    | NotKnown
    | MaybeKnown


toggle : comparable -> Dict.Dict comparable WordKnowledge -> Dict.Dict comparable WordKnowledge
toggle key =
    Dict.update key
        (\old ->
            case old of
                Just value ->
                    case value of
                        NotKnown ->
                            Just MaybeKnown

                        MaybeKnown ->
                            Just Known

                        Known ->
                            Just NotKnown

                Nothing ->
                    Nothing
        )


words : List ( String, WordKnowledge )
words =
    [ ( "crave", NotKnown )
    ]
