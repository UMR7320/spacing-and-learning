module Session3.CU3 exposing (..)

import Data
import Dict
import ExperimentInfo
import Html.Styled as Html exposing (Html, div, fromUnstyled, span, text)
import Html.Styled.Attributes exposing (class)
import Html.Styled.Events
import Http exposing (Error)
import Icons
import Json.Decode as Decode exposing (Decoder, string)
import Json.Decode.Pipeline exposing (..)
import Logic
import Progressbar exposing (progressBar)
import Session1.CU1 exposing (CU1Msg(..))
import View


view exp { userClickedAudio, toggleFeedback, nextTrialMsg, startMainMsg, userChangedInput, saveDataMsg } =
    case exp of
        Logic.NotStarted ->
            div [] [ text "Task is not started yet." ]

        Logic.Loading ->
            div [] [ text "Loading..." ]

        Logic.Err reason ->
            div [] [ text <| "I stumbled into an error : " ++ reason ]
        Logic.Running Logic.Instructions data ->
            div [] []

        Logic.Running Logic.Training ({ trainingTrials, mainTrials, current, state, feedback, history } as data) ->
            case current of
                Just trial ->
                    div []
                        [ View.viewTraining data.infos.instructions
                            [ Html.p [ class "max-w-lg  text-lg m-4" ]
                                [ View.fromMarkdown trial.context
                                , Html.br [] []
                                ]
                            , div [ class "flex flex-row p-4 text-lg items-center" ]
                                [ View.fromMarkdown trial.amorce
                                , View.floatingLabel "" state.userAnswer userChangedInput feedback
                                ]
                            , View.genericNeutralFeedback
                                { isVisible = feedback
                                , feedback_Correct = ( trial.feedback, [] )
                                , button = View.navigationButton toggleFeedback nextTrialMsg feedback
                                }
                            ]
                        ]

                Nothing ->
                    View.introToMain (startMainMsg mainTrials data.infos)

        Logic.Running Logic.Main ({ mainTrials, current, state, feedback, history } as data) ->
            case current of
                Just trial ->
                    div [ class "flex flex-col items-center" ]
                        [ View.tooltip data.infos.instructions_short
                        , progressBar history mainTrials
                        , Html.p [ class "max-w-lg  text-lg m-4" ]
                            [ View.fromMarkdown trial.context
                            , Html.br [] []
                            ]
                        , div [ class "flex flex-row p-4 text-lg items-center" ]
                            [ View.fromMarkdown trial.amorce
                            , View.floatingLabel "" state.userAnswer userChangedInput feedback
                            ]
                        , View.genericNeutralFeedback
                            { isVisible = feedback
                            , feedback_Correct = ( trial.feedback, [] )
                            , button = View.navigationButton toggleFeedback nextTrialMsg feedback
                            }
                        ]

                Nothing ->
                    View.end data.infos.end saveDataMsg "/"


type Msg
    = UserClickedNextTrial
    | UserClickedToggleFeedback
    | UserClickedRadioButton String
    | UserClickedStartMain (List Trial) ExperimentInfo.Task
    | UserChangedInput String
    | UserClickedSaveData
    | ServerRespondedWithLastRecords (Result Http.Error (List ()))


getTrialsFromServer : (Result Error (List Trial) -> msg) -> Cmd msg
getTrialsFromServer msgHandler =
    Data.getTrialsFromServer_ "input" "ContextUnderstandingLvl3" msgHandler decodeTranslationInput


start : List ExperimentInfo.Task -> List Trial -> Logic.Task Trial State
start info trials =
    let
        relatedInfos =
            Dict.get taskId (ExperimentInfo.toDict info) |> Result.fromMaybe ("I couldn't fetch the value associated with: " ++ taskId)
    in
    Logic.startIntro relatedInfos
        (List.filter (\datum -> datum.isTraining) trials)
        (List.filter (\datum -> not datum.isTraining) trials)
        initState


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


taskId =
    "recFEtKbtuBSolHnI"


getRecords =
    Http.task
        { method = "GET"
        , headers = []
        , url =
            Data.buildQuery
                { app = Data.apps.spacing
                , base = "input"
                , view_ = "Presentation"
                }
        , body = Http.emptyBody
        , resolver = Http.stringResolver <| Data.handleJsonResponse <| decodeTranslationInput
        , timeout = Just 5000
        }
