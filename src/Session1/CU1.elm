module Session1.CU1 exposing (..)

import Data
import Dict
import ExperimentInfo exposing (Task)
import Html.Styled as Html exposing (Html, div, fromUnstyled, h2, p, span, text)
import Html.Styled.Attributes exposing (class)
import Html.Styled.Events
import Http exposing (Error)
import Icons
import Json.Decode as Decode exposing (Decoder, string)
import Json.Decode.Pipeline exposing (..)
import Logic
import Progressbar
import String.Interpolate exposing (interpolate)
import View



--view : { a | task : Logic.Task t s, optionsOrder : Maybe (List comparable), nextTrialMsg : f, userClickedAudio : String -> f, radioMsg : String -> f, toggleFeedbackMsg : f, startMainMsg : List t -> f, inputChangedMsg : String -> f } -> Html msg


taskId =
    "recsN8oyy3LIC8URx"


paragraphWithInput pre userAnswer post =
    p [ class "max-w-3xl text-lg p-4" ]
        [ text pre
        , span [ class "border-4 w-24 h-2 pl-4 pr-4 font-bold" ] [ text userAnswer ]
        , text post
        ]


view :
    { task : Logic.Task Trial State
    , infos : Maybe ExperimentInfo.Task
    , radioMsg : String -> msg
    , toggleFeedbackMsg : msg
    , nextTrialMsg : msg
    , optionsOrder : List comparable
    , startMainMsg : List Trial -> Task -> msg
    , userClickedSaveData : msg
    }
    -> Html msg
view task =
    case task.task of
        Logic.NotStarted ->
            div [] [ text "experiment did not start yet" ]

        Logic.Err reason ->
            div [] [ text reason ]

        Logic.Running Logic.Training data ->
            case data.current of
                Just trial ->
                    let
                        ( pre, post ) =
                            case String.split "/" trial.text of
                                x :: y :: _ ->
                                    ( x, y )

                                [ x ] ->
                                    ( x, "defaultPost" )

                                [] ->
                                    ( "defautpre", "defaultpOst" )
                    in
                    div []
                        [ View.viewTraining data.infos.instructions
                            [ View.trainingWheelsGeneric (List.length data.history) data.infos.trainingWheel [ View.bold trial.target ]
                            , paragraphWithInput pre data.state.userAnswer post
                            , div [ class "w-full max-w-2xl" ] <| View.shuffledOptions data.state data.feedback task.radioMsg trial task.optionsOrder
                            , div [ class "col-start-2 col-span-4" ] <|
                                [ View.genericSingleChoiceFeedback
                                    { isVisible = data.feedback
                                    , userAnswer = data.state.userAnswer
                                    , target = trial.target
                                    , feedback_Correct = ( data.infos.feedback_correct, [ View.bold trial.target, View.bold trial.definition ] )
                                    , feedback_Incorrect = ( data.infos.feedback_incorrect, [ View.bold trial.target, View.bold trial.definition ] )
                                    , button = View.navigationButton task.toggleFeedbackMsg task.nextTrialMsg data.feedback
                                    }
                                ]
                            ]
                        ]

                Nothing ->
                    View.introToMain (task.startMainMsg data.mainTrials data.infos)

        Logic.Running Logic.Main data ->
            case data.current of
                Just trial ->
                    let
                        ( pre, post ) =
                            case String.split "/" trial.text of
                                x :: y :: _ ->
                                    ( x, y )

                                [ x ] ->
                                    ( x, "defaultPost" )

                                [] ->
                                    ( "defautpre", "defaultpOst" )
                    in
                    div [ class "container flex flex-col w-full w-max-3xl items-center justify-center " ]
                        [ Progressbar.progressBar data.history data.mainTrials
                        , View.tooltip data.infos.instructions_short
                        , paragraphWithInput pre data.state.userAnswer post
                        , div [ class "w-full max-w-xl" ] <| View.shuffledOptions data.state data.feedback task.radioMsg trial task.optionsOrder
                        , View.genericSingleChoiceFeedback
                            { isVisible = data.feedback
                            , userAnswer = data.state.userAnswer
                            , target = trial.target
                            , feedback_Correct = ( data.infos.feedback_correct, [ View.bold trial.target, View.bold trial.definition ] )
                            , feedback_Incorrect = ( data.infos.feedback_incorrect, [ View.bold trial.target, View.bold trial.definition ] )
                            , button = View.navigationButton task.toggleFeedbackMsg task.nextTrialMsg data.feedback
                            }
                        ]

                Nothing ->
                    View.end data.infos.end task.userClickedSaveData "./"

        Logic.Loading ->
            text "Loading..."

        Logic.Running Logic.Instructions data ->
            text ""


type CU1Msg
    = UserClickedNextTrial
    | UserClickedToggleFeedback
    | UserClickedRadioButton String
    | UserClickedStartMain (List Trial) ExperimentInfo.Task
    | UserClickedSaveData
    | ServerRespondedWithLastRecords (Result Http.Error (List ()))


getTrialsFromServer : (Result Error (List Trial) -> msg) -> Cmd msg
getTrialsFromServer msgHandler =
    Data.getTrialsFromServer_ "input" "ContextUnderstandingLvl1" msgHandler decodeTranslationInput


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
                |> required "Text_To_Complete" string
                |> required "Word_Text" string
                |> required "Distractor_1_CU_Lvl1" string
                |> required "Distractor_2_CU_Lvl1" string
                |> required "Distractor_3_CU_Lvl1" string
                |> required "Definition" string
                |> Data.decodeBool "isTraining"
    in
    Data.decodeRecords decoder


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


initState : State
initState =
    State "DefaultUid" ""


defaultTrial : Trial
defaultTrial =
    Trial "defaultuid" "defaulttarger" "defaultText" "distractor1" "distractor2" "distractor3" "definition" False


type alias Trial =
    { uid : String
    , text : String
    , target : String
    , distractor1 : String
    , distractor2 : String
    , distractor3 : String
    , definition : String
    , isTraining : Bool
    }


type alias State =
    { uid : String
    , userAnswer : String
    }
