module Experiment.Spelling3 exposing (..)

import Data
import ExperimentInfo
import Html.Styled as Html exposing (Html, div, fromUnstyled, h1, p, span, text)
import Html.Styled.Attributes exposing (class)
import Html.Styled.Events
import Http exposing (Error)
import Icons
import Json.Decode as Decode exposing (Decoder, string)
import Json.Decode.Pipeline exposing (..)
import PsychTask
import View


view exp infos { userClickedAudio, toggleFeedback, nextTrialMsg, startMainMsg, userChangedInput } =
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
                            [ div [ class "col-start-2 col-span-4" ] [ text trial.context ]
                            , div [ class "h-12 w-12", Html.Styled.Events.onClick (userClickedAudio trial.audioSentence.url) ] [ fromUnstyled <| Icons.music ]
                            , div [ class "col-start-2 col-span-4" ]
                                [ text trial.amorce
                                , View.floatingLabel "" state.userAnswer userChangedInput feedback
                                ]
                            , div [ class "col-start-2 col-span-4" ] <|
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
                        [ text trial.context
                        , div [ class "h-12 w-12", Html.Styled.Events.onClick (userClickedAudio trial.audioSentence.url) ] [ fromUnstyled <| Icons.music ]
                        , div [ class "flex flex-row" ]
                            [ span [ class "pr-4" ] [ text trial.amorce ]
                            , View.floatingLabel "" state.userAnswer userChangedInput feedback
                            ]
                        , if feedback then
                            div []
                                [ text "I'm feedback"
                                , View.button
                                    { message = nextTrialMsg
                                    , isDisabled = False
                                    , txt = "Next Item"
                                    }
                                ]

                          else
                            View.button
                                { message = toggleFeedback
                                , isDisabled = False
                                , txt = "Check my answer"
                                }
                        ]

                Nothing ->
                    div [] [ h1 [] [ text "Congrats 🎉️" ], p [] [ text info.end ] ]

        ( PsychTask.Over, Just info ) ->
            div [] [ h1 [] [ text "Congrats" ], p [] [ text info.end ] ]


type Msg
    = UserClickedNextTrial
    | UserClickedToggleFeedback
    | ServerRespondedWith (Result Http.Error (List Trial))
    | UserClickedStartIntro (List Trial)
    | UserClickedStartMain (List Trial)
    | UserChangedInput String


getTrialsFromServer : (Result Error (List Trial) -> msg) -> Cmd msg
getTrialsFromServer msgHandler =
    Data.getTrialsFromServer_ "input" "ContextUnderstandingLvl3" msgHandler decodeTranslationInput


decodeTranslationInput : Decoder (List Trial)
decodeTranslationInput =
    let
        decoder =
            Decode.succeed Trial
                |> required "UID" string
                |> required "Word_Text" string
                |> optional "CU_Lv3_Audio" Data.decodeAudioFiles (Data.AudioFile "" "")
                |> required "CU_Lvl3_Presentation" string
                |> required "CU_Lvl3_TextToComplete_amorce" string
                |> required "CU_Lvl3_Feedback" string
                |> Data.decodeBool "isTraining"
    in
    Data.decodeRecords decoder


initState : State
initState =
    State "DefaultUid" ""


defaultTrial : Trial
defaultTrial =
    { uid = ""
    , writtenWord = "String"
    , audioSentence = Data.AudioFile "" ""
    , context = "String"
    , amorce = "String"
    , feedback = "String"
    , isTraining = False
    }


type alias Trial =
    { uid : String
    , writtenWord : String
    , audioSentence : Data.AudioFile
    , context : String
    , amorce : String
    , feedback : String
    , isTraining : Bool
    }


type alias State =
    { uid : String
    , userAnswer : String
    }
