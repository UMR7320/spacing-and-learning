module Postest.CloudWords exposing (State, WordKnowledge, toggle, words)

import Browser
import Dict
import Html exposing (Html)
import Html.Styled as H exposing (toUnstyled)
import Html.Styled.Attributes as Attr


type alias Model =
    { words : Dict.Dict String Bool }


type State
    = State (Dict.Dict String Bool)


type Msg
    = ToggleWord String


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


view : Model -> Html Msg
view model =
    H.div []
        (List.map
            (\word ->
                let
                    value =
                        Dict.get word model.words
                in
                H.label [ Attr.class "checkbox" ] [ H.input [] [] ]
            )
            (Dict.keys model.words)
        )
        |> toUnstyled


words : List ( String, WordKnowledge )
words =
    [ ( "crave", NotKnown )
    ]
