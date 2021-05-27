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
import Ports
import Progressbar exposing (progressBar)
import Random
import Random.List
import Session1.Meaning exposing (Msg(..))
import Session3.CU3 exposing (Msg(..))
import View


view exp optionsOrder =
    case exp of
        Logic.NotStarted ->
            div [] [ text "experiment did not start yet" ]

        Logic.Loading ->
            div [] [ text "Loading..." ]

        Logic.Err reason ->
            div [] [ text reason ]

        Logic.Running Logic.Instructions data ->
            div [] [ View.instructions data.infos.instructions UserClickedStartTraining ]

        Logic.Running Logic.Training ({ trainingTrials, mainTrials, current, state, feedback, history } as data) ->
            case current of
                Just trial ->
                    div [ class "flex flex-col items-center" ]
                        [ Html.p [ class "p-4" ] [ text trial.context ]
                        , View.audioButton UserClickedAudio trial.audioSentence.url "dialog"
                        , div [] <| View.shuffledOptions state feedback UserClickedRadioButton trial optionsOrder
                        , View.genericSingleChoiceFeedback
                            { isVisible = feedback
                            , userAnswer = state.userAnswer
                            , target = trial.target
                            , feedback_Correct = ( trial.feedback, [] )
                            , feedback_Incorrect = ( trial.feedback, [] )
                            , button = View.navigationButton UserClickedToggleFeedback UserClickedNextTrial feedback
                            }
                        ]

                Nothing ->
                    View.introToMain (UserClickedStartMain mainTrials data.infos)

        Logic.Running Logic.Main ({ mainTrials, current, state, feedback, history } as data) ->
            case current of
                Just trial ->
                    div [ class "flex flex-col w-full items-center" ]
                        [ View.tooltip data.infos.instructions_short
                        , progressBar history mainTrials
                        , Html.p [ class "p-8 text-lg" ] [ text trial.context ]
                        , View.audioButton UserClickedAudio trial.audioSentence.url "dialog"
                        , div [ class "max-w-2xl pt-4" ] <| View.shuffledOptions state feedback UserClickedRadioButton trial optionsOrder
                        , View.genericSingleChoiceFeedback
                            { isVisible = feedback
                            , userAnswer = state.userAnswer
                            , target = trial.target
                            , feedback_Correct = ( trial.feedback, [] )
                            , feedback_Incorrect = ( trial.feedback, [] )
                            , button = View.navigationButton UserClickedToggleFeedback UserClickedNextTrial feedback
                            }
                        ]

                Nothing ->
                    View.end data.infos.end UserClickedSaveData "/"


update msg model =
    case msg of
        UserClickedNextTrial ->
            ( { model | cuLvl2 = Logic.next initState model.cuLvl2 }, Random.generate RuntimeShuffledOptionsOrder (Random.List.shuffle model.optionsOrder) )

        UserClickedToggleFeedback ->
            ( { model | cuLvl2 = Logic.toggle model.cuLvl2 }, Cmd.none )

        UserClickedRadioButton newChoice ->
            ( { model | cuLvl2 = Logic.update { uid = "", userAnswer = newChoice } model.cuLvl2 }, Cmd.none )

        UserClickedStartMain _ _ ->
            ( { model | cuLvl2 = Logic.startMain model.cuLvl2 initState }, Cmd.none )

        ServerRespondedWithLastRecords _ ->
            ( model, Cmd.none )

        UserClickedSaveData ->
            let
                responseHandler =
                    ServerRespondedWithLastRecords
            in
            ( model, Logic.saveData responseHandler model.user taskId model.cuLvl2 )

        UserClickedAudio url ->
            ( model, Ports.playAudio url )

        RuntimeShuffledOptionsOrder ls ->
            ( { model | optionsOrder = ls }, Cmd.none )

        UserClickedStartTraining ->
            ( { model | cuLvl2 = Logic.startTraining model.cuLvl2 }, Cmd.none )


type CU2Msg
    = UserClickedNextTrial
    | UserClickedToggleFeedback
    | UserClickedRadioButton String
    | UserClickedStartMain (List Trial) ExperimentInfo.Task
    | UserClickedSaveData
    | ServerRespondedWithLastRecords (Result Http.Error (List ()))
    | UserClickedAudio String
    | RuntimeShuffledOptionsOrder (List Int)
    | UserClickedStartTraining


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
