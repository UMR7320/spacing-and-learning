module Postest.CloudWords exposing (State, toggle, words)

import Browser
import Dict
import Html exposing (Html)
import Html.Styled as H exposing (toUnstyled)
import Html.Styled.Attributes as Attr


main : Program () Model Msg
main =
    Browser.element
        { init = init
        , subscriptions = \_ -> Sub.none
        , update = update
        , view = view
        }


init : () -> ( Model, Cmd Msg )
init _ =
    ( { words = Dict.fromList words }, Cmd.none )


type alias Model =
    { words : Dict.Dict String Bool }


type State
    = State (Dict.Dict String Bool)


type Msg
    = ToggleWord String


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        ToggleWord word ->
            ( { model | words = toggle word model.words }, Cmd.none )


toggle : comparable -> Dict.Dict comparable Bool -> Dict.Dict comparable Bool
toggle key =
    Dict.update key
        (\old ->
            case old of
                Just value ->
                    Just (not value)

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


words : List ( String, Bool )
words =
    [ ( "crave", False )
    , ( "curb", False )
    , ( "dazzle", False )
    , ( "divert", False )
    , ( "dread", False )
    , ( "equate", False )
    , ( "hail", False )
    , ( "hinder", False )
    , ( "hum", False )
    , ( "incur", False )
    , ( "intrude", False )
    , ( "juggle", False )
    , ( "loathe", False )
    , ( "mingle", False )
    , ( "moan", False )
    , ( "pinpoint", False )
    , ( "ponder", False )
    , ( "refrain", False )
    , ( "relish", False )
    , ( "saddle", False )
    , ( "seduce", False )
    , ( "sprinkle", False )
    , ( "strive", False )
    , ( "uphold", False )
    , ( "vow", False )
    , ( "wield", False )
    ]
