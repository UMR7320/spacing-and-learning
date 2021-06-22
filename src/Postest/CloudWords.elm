module Postest.CloudWords exposing (Msg(..), WordKnowledge, toggle, update, view, words)

import Dict
import Html.Styled exposing (div, input, label, span, text)
import Html.Styled.Attributes exposing (class, type_)
import Html.Styled.Events


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
    , ( "crave", NotKnown )
    , ( "curb", NotKnown )
    , ( "dazzle", NotKnown )
    , ( "divert", NotKnown )
    , ( "dread", NotKnown )
    , ( "equate", NotKnown )
    , ( "hail", NotKnown )
    , ( "hinder", NotKnown )
    , ( "hum", NotKnown )
    , ( "incur", NotKnown )
    , ( "intrude", NotKnown )
    , ( "juggle", NotKnown )
    , ( "loathe", NotKnown )
    , ( "mingle", NotKnown )
    , ( "moan", NotKnown )
    , ( "pinpoint", NotKnown )
    , ( "ponder", NotKnown )
    , ( "refrain", NotKnown )
    , ( "relish", NotKnown )
    , ( "saddle", NotKnown )
    , ( "seduce", NotKnown )
    , ( "sprinkle", NotKnown )
    , ( "strive", NotKnown )
    , ( "uphold", NotKnown )
    , ( "vow", NotKnown )
    , ( "wield", NotKnown )
    , ( "withstand", NotKnown )
    ]


type Msg
    = UserToggledInCloudWords String


update msg model =
    case msg of
        UserToggledInCloudWords word ->
            ( { model | cloudWords = toggle word model.cloudWords }, Cmd.none )


view model =
    div [ class "grid grid-flow-col content-center grid-rows-4 auto-cols-max gap-4 " ]
        (Dict.map
            (\word k ->
                div
                    [ class "transition duration-300 ease-in-out rounded-lg cursor-pointer p-2 align-baseline flex flex-row"
                    , class
                        (if k == NotKnown then
                            "bg-red-500"

                         else if k == MaybeKnown then
                            ""

                         else
                            "bg-green-500"
                        )
                    , Html.Styled.Events.onClick <|
                        UserToggledInCloudWords word
                    ]
                    [ text <|
                        if k == NotKnown then
                            "👎"

                        else if k == MaybeKnown then
                            "🤔"

                        else
                            "👍"
                    , span [ class "pl-2 text-lg" ] [ text word ]
                    ]
            )
            model.cloudWords
            |> Dict.values
        )
