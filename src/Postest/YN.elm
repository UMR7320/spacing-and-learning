module Postest.YN exposing (..)

import Data
import ExperimentInfo exposing (Task)
import Html.Styled as Html exposing (div, span, text)
import Html.Styled.Attributes exposing (class)
import Html.Styled.Events
import Http exposing (Error)
import Json.Decode as Decode exposing (Decoder, string)
import Json.Decode.Pipeline exposing (..)
import Logic
import View


view exp { toggleFeedback, nextTrialMsg, startMainMsg, userChangedInput } =
    case exp of
        Logic.NotStarted ->
            div [] [ text "experiment did not start yet" ]

        Logic.Loading ->
            div [] [ text "Loading..." ]

        Logic.Err reason ->
            div [] [ text reason ]

        Logic.Running Logic.Instructions _ ->
            div [] []

        Logic.Running Logic.Training { mainTrials, current, feedback, infos } ->
            case current of
                Just _ ->
                    div []
                        [ View.viewTraining infos.instructions
                            [ div [] <|
                                if feedback then
                                    [ text "I'm feedback"
                                    , View.button
                                        { message = nextTrialMsg
                                        , isDisabled = False
                                        , txt = "Next Training Item"
                                        }
                                    ]

                                else
                                    [ View.button
                                        { message = toggleFeedback
                                        , isDisabled = False
                                        , txt = "togglefeedback"
                                        }
                                    ]
                            ]
                        ]

                Nothing ->
                    div []
                        [ text "Intro is over"
                        , View.button
                            { message = startMainMsg mainTrials infos
                            , isDisabled = False
                            , txt = "Start"
                            }
                        ]

        Logic.Running Logic.Main { current, state, infos } ->
            case current of
                Just trial ->
                    div []
                        [ span [ class "text-lg" ] [ text trial.word ]
                        , Html.fieldset [ class "flex flex-col" ]
                            [ div []
                                [ Html.input
                                    [ Html.Styled.Attributes.type_ "radio"
                                    , Html.Styled.Attributes.value "true"
                                    , Html.Styled.Attributes.checked (state.userAnswer == "true")
                                    , Html.Styled.Events.onInput userChangedInput
                                    , Html.Styled.Attributes.id "truecb"
                                    ]
                                    []
                                , Html.label [ Html.Styled.Attributes.for "truecb", class "pl-4 hover:underline" ] [ text "Exists" ]
                                ]
                            , div []
                                [ Html.input
                                    [ Html.Styled.Attributes.type_ "radio"
                                    , Html.Styled.Attributes.value "false"
                                    , Html.Styled.Attributes.checked (state.userAnswer == "false")
                                    , Html.Styled.Events.onInput userChangedInput
                                    , Html.Styled.Attributes.id "falsecb"
                                    ]
                                    []
                                , Html.label [ Html.Styled.Attributes.for "falsecb", class "pl-4 hover:underline" ] [ text "Does not exist" ]
                                ]
                            ]
                        , div [] <|
                            [ View.button
                                { message = nextTrialMsg
                                , isDisabled = False
                                , txt = "Next Item"
                                }
                            ]
                        ]

                Nothing ->
                    text infos.end


type Msg
    = UserClickedNextTrial
    | UserClickedToggleFeedback
    | UserClickedStartIntro (List Trial)
    | UserClickedStartMain (List Trial) Task
    | UserChangedInput String


getTrialsFromServer : (Result Error (List Trial) -> msg) -> Cmd msg
getTrialsFromServer msgHandler =
    Data.getTrialsFromServer_ "yes_no" "all" msgHandler decodeTranslationInput


decodeTranslationInput : Decoder (List Trial)
decodeTranslationInput =
    let
        decoder =
            Decode.succeed Trial
                |> required "uid" string
                |> required "word" string
                |> Data.decodeBool "exists"
    in
    Data.decodeRecords decoder


initState : State
initState =
    State "DefaultUid" ""


defaultTrial : Trial
defaultTrial =
    { uid = ""
    , word = "String"
    , exists = False
    }


type alias Trial =
    { uid : String
    , word : String
    , exists : Bool
    }


type alias State =
    { uid : String
    , userAnswer : String
    }
