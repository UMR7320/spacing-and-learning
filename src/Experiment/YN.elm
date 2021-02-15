module Experiment.YN exposing (..)

import Data
import ExperimentInfo
import Html.Styled as Html exposing (Html, div, fromUnstyled, span, text)
import Html.Styled.Attributes exposing (class)
import Html.Styled.Events
import Http exposing (Error)
import Icons
import Json.Decode as Decode exposing (Decoder, string)
import Json.Decode.Pipeline exposing (..)
import PsychTask
import View


view exp infos { toggleFeedback, nextTrialMsg, startMainMsg, userChangedInput } =
    case ( exp, infos ) of
        ( _, Nothing ) ->
            div [] [ text "Error : I can't find the task's infos" ]

        ( PsychTask.NotStartedYet, Just info ) ->
            div [] [ text "experiment did not start yet" ]

        ( PsychTask.Intr { trainingTrials, mainTrials, current, state, feedback, history }, Just info ) ->
            case current of
                Just trial ->
                    div []
                        [ View.viewTraining info.instructions
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
                            { message = startMainMsg mainTrials
                            , isDisabled = False
                            , txt = "Start"
                            }
                        ]

        ( PsychTask.IntroOver, Just info ) ->
            text "L'entrainement est fini"

        ( PsychTask.Main { mainTrials, current, state, feedback, history }, Just info ) ->
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
                    text info.end

        ( PsychTask.Over, Just info ) ->
            text "I'm over"


type Msg
    = UserClickedNextTrial
    | UserClickedToggleFeedback
    | ServerRespondedWith (Result Http.Error (List Trial))
    | UserClickedStartIntro (List Trial)
    | UserClickedStartMain (List Trial)
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
