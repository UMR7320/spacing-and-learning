module Session2.CU2 exposing (..)

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
import Markdown
import Progressbar exposing (progressBar)
import Session1.CU1 exposing (CU1Msg(..))
import View


view exp optionsOrder { userClickedAudio, radioMsg, toggleFeedback, nextTrialMsg, startMainMsg, saveData } =
    case exp of
        Logic.NotStarted ->
            div [] [ text "experiment did not start yet" ]

        Logic.Loading ->
            div [] [ text "Loading..." ]

        Logic.Err reason ->
            div [] [ text reason ]

        Logic.Intr ({ trainingTrials, mainTrials, current, state, feedback, history } as data) ->
            case current of
                Just trial ->
                    div []
                        [ View.viewTraining data.infos.instructions
                            [ Html.p [ class "p-4" ] [ text trial.context ]
                            , View.audioButton userClickedAudio trial.audioSentence.url "dialog"
                            , div [] <| View.shuffledOptions state feedback radioMsg trial optionsOrder
                            , View.genericSingleChoiceFeedback
                                { isVisible = feedback
                                , userAnswer = state.userAnswer
                                , target = trial.target
                                , feedback_Correct = ( trial.feedback, [] )
                                , feedback_Incorrect = ( trial.feedback, [] )
                                , button = View.navigationButton toggleFeedback nextTrialMsg feedback
                                }
                            ]
                        ]

                Nothing ->
                    View.introToMain (startMainMsg mainTrials data.infos)

        Logic.Main ({ mainTrials, current, state, feedback, history } as data) ->
            case current of
                Just trial ->
                    div [ class "flex flex-col w-full items-center" ]
                        [ View.tooltip data.infos.instructions_short
                        , progressBar history mainTrials
                        , Html.p [ class "p-8 text-lg" ] [ text trial.context ]
                        , View.audioButton userClickedAudio trial.audioSentence.url "dialog"
                        , div [ class "max-w-2xl pt-4" ] <| View.shuffledOptions state feedback radioMsg trial optionsOrder
                        , View.genericSingleChoiceFeedback
                            { isVisible = feedback
                            , userAnswer = state.userAnswer
                            , target = trial.target
                            , feedback_Correct = ( trial.feedback, [] )
                            , feedback_Incorrect = ( trial.feedback, [] )
                            , button = View.navigationButton toggleFeedback nextTrialMsg feedback
                            }
                        ]

                Nothing ->
                    View.end data.infos.end saveData "/"


type CU2Msg
    = UserClickedNextTrial
    | UserClickedToggleFeedback
    | UserClickedRadioButton String
    | UserClickedStartMain (List Trial) ExperimentInfo.Task
    | UserClickedSaveData
    | ServerRespondedWithLastRecords (Result Http.Error (List String))


getTrialsFromServer : (Result Error (List Trial) -> msg) -> Cmd msg
getTrialsFromServer msgHandler =
    Data.getTrialsFromServer_ "input" "SpellingLvl2" msgHandler decodeTranslationInput


decodeTranslationInput : Decoder (List Trial)
decodeTranslationInput =
    let
        decoder =
            Decode.succeed Trial
                |> required "UID" string
                |> required "Word_Text" string
                |> optional "Audio_Understanding" Data.decodeAudioFiles (Data.AudioFile "" "")
                |> required "CU_Lvl1_Context" string
                |> required "CU_Lvl2_target" string
                |> required "CU_Lvl2_Distractor_1" string
                |> required "CU_Lvl2_Distractor_2" string
                |> required "CU_Lvl2_Distractor_3" string
                |> required "Feedback_CU_Lvl2" string
                |> Data.decodeBool "isTraining"
    in
    Data.decodeRecords decoder


initState : State
initState =
    State "DefaultUid" ""


defaultTrial : Trial
defaultTrial =
    Trial "defaultTrial" "defaultTrial" (Data.AudioFile "" "") "defautcontext" "defaulttarget" "defautdis1" "defaultdis2" "defaultdis3" "defaultfeedback" False


type alias Trial =
    { uid : String
    , writtenWord : String
    , audioSentence : Data.AudioFile
    , context : String
    , target : String
    , distractor1 : String
    , distractor2 : String
    , distractor3 : String
    , feedback : String
    , isTraining : Bool
    }


type alias State =
    { uid : String
    , userAnswer : String
    }


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


taskId =
    "recwxsmowpB18bpLj"


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
