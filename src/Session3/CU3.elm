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
import Random
import Random.List
import Session3.Spelling3 exposing (Msg(..))
import View


view exp =
    case exp of
        Logic.NotStarted ->
            div [] [ text "Task is not started yet." ]

        Logic.Loading ->
            div [] [ text "Loading..." ]

        Logic.Err reason ->
            div [] [ text <| "I stumbled into an error : " ++ reason ]

        Logic.Running Logic.Instructions data ->
            div [] [ View.instructions data.infos.instructions UserClickedStartTraining ]

        Logic.Running Logic.Training ({ current, state, feedback } as data) ->
            case current of
                Just trial ->
                    div [ class "flex flex-col items-center" ]
                        [ Html.p [ class "max-w-lg  text-lg m-4" ]
                            [ View.fromMarkdown trial.context
                            , Html.br [] []
                            ]
                        , div [ class "flex flex-row p-4 text-lg items-center" ]
                            [ span [ class "pr-4" ] [ View.fromMarkdown trial.amorce ]
                            , View.floatingLabel "" state.userAnswer UserChangedInput feedback
                            ]
                        , View.genericNeutralFeedback
                            { isVisible = feedback
                            , feedback_Correct = ( trial.feedback, [] )
                            , button = View.navigationButton UserClickedToggleFeedback UserClickedNextTrial feedback
                            }
                        ]

                Nothing ->
                    View.introToMain UserClickedStartMain

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
                            [ span [ class "pr-4" ] [ View.fromMarkdown trial.amorce ]
                            , View.floatingLabel "" state.userAnswer UserChangedInput feedback
                            ]
                        , View.genericNeutralFeedback
                            { isVisible = feedback
                            , feedback_Correct = ( trial.feedback, [] )
                            , button = View.navigationButton UserClickedToggleFeedback UserClickedNextTrial feedback
                            }
                        ]

                Nothing ->
                    View.end data.infos.end UserClickedSaveData "/"


type Msg
    = UserClickedNextTrial
    | UserClickedToggleFeedback
    | UserClickedRadioButton String
    | UserClickedStartMain
    | UserChangedInput String
    | UserClickedSaveData
    | ServerRespondedWithLastRecords (Result Http.Error (List ()))
    | UserClickedStartTraining
    | RuntimeShuffledOptionsOrder (List Int)


update msg model =
    case msg of
        UserClickedNextTrial ->
            ( { model | cu3 = Logic.next initState model.cu3 }, Random.generate RuntimeShuffledOptionsOrder (Random.List.shuffle model.optionsOrder) )

        UserClickedToggleFeedback ->
            ( { model | cu3 = Logic.toggle model.cu3 }, Cmd.none )

        UserClickedRadioButton newChoice ->
            ( { model | cu3 = Logic.update { uid = "", userAnswer = newChoice } model.cu3 }, Cmd.none )

        UserClickedStartMain ->
            ( { model | cu3 = Logic.startMain model.cu3 initState }, Cmd.none )

        UserChangedInput new ->
            ( { model | cu3 = Logic.update { uid = "", userAnswer = new } model.cu3 }, Cmd.none )

        UserClickedSaveData ->
            let
                responseHandler =
                    ServerRespondedWithLastRecords
            in
            ( model, Logic.saveData responseHandler model.user taskId model.scrabbleTask )

        ServerRespondedWithLastRecords _ ->
            ( model, Cmd.none )

        UserClickedStartTraining ->
            ( { model | cu3 = Logic.startTraining model.cu3 }, Cmd.none )

        RuntimeShuffledOptionsOrder ls ->
            ( { model | optionsOrder = ls }, Cmd.none )


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
