module Session2.CU2 exposing (..)

import Data
import Delay
import Dict
import ExperimentInfo
import Html.Styled as Html exposing (div, text)
import Html.Styled.Attributes exposing (class)
import Http exposing (Error)
import Json.Decode as Decode exposing (Decoder, string)
import Json.Decode.Pipeline exposing (..)
import Logic
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

        Logic.Running Logic.Training ({ mainTrials, current, state, feedback } as data) ->
            case ( current, data.state.step ) of
                ( Just trial, Listening nTimes ) ->
                    div [ class "flex flex-col text-lg items-center" ]
                        [ Html.p [ class "p-4" ] [ View.fromMarkdown trial.context ]
                        , div [ class "flex flex-row" ]
                            [ if nTimes > 0 then
                                View.audioButton UserClickedAudio trial.audioSentence.url ("dialog " ++ String.fromInt nTimes)

                              else
                                View.button { isDisabled = nTimes == 0, message = UserClickedStartAnswering, txt = "What happened ?" }
                            , View.button
                                { isDisabled =
                                    nTimes == 3
                                , message = UserClickedStartAnswering
                                , txt = "Now choose the best description"
                                }
                            ]
                        ]

                ( Just trial, Answering ) ->
                    div [ class "flex flex-col items-center" ]
                        [ div [] <| View.shuffledOptions state feedback UserClickedRadioButton trial optionsOrder
                        , View.genericSingleChoiceFeedback
                            { isVisible = feedback
                            , userAnswer = state.userAnswer
                            , target = trial.target
                            , feedback_Correct = ( trial.feedback, [] )
                            , feedback_Incorrect = ( trial.feedback, [] )
                            , button = View.navigationButton UserClickedToggleFeedback UserClickedNextTrial feedback
                            }
                        ]

                ( Nothing, _ ) ->
                    View.introToMain (UserClickedStartMain mainTrials data.infos)

        Logic.Running Logic.Main ({ mainTrials, current, state, feedback, history } as data) ->
            case ( current, data.state.step ) of
                ( Just trial, Listening nTimes ) ->
                    div [ class "flex flex-col w-full items-center" ]
                        [ View.tooltip data.infos.instructions_short
                        , progressBar history mainTrials
                        , Html.p [ class "p-8 text-lg" ] [ View.fromMarkdown trial.context ]
                        , if nTimes > 0 then
                            View.audioButton UserClickedAudio trial.audioSentence.url ("dialog " ++ String.fromInt nTimes)

                          else
                            View.button { isDisabled = nTimes == 0, message = UserClickedStartAnswering, txt = "What happened ?" }
                        , View.button
                            { isDisabled =
                                nTimes == 3
                            , message = UserClickedStartAnswering
                            , txt = "What happened?"
                            }
                        ]

                ( Just trial, Answering ) ->
                    div [ class "flex flex-col w-full items-center" ]
                        [ View.tooltip data.infos.instructions_short
                        , progressBar history mainTrials
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

                ( Nothing, _ ) ->
                    View.end data.infos.end UserClickedSaveData "/"


update msg model =
    let
        prevState =
            Logic.getState model.cuLvl2 |> Maybe.withDefault initState
    in
    case msg of
        UserClickedNextTrial ->
            ( { model | cuLvl2 = Logic.next initState model.cuLvl2 }, Random.generate RuntimeShuffledOptionsOrder (Random.List.shuffle model.optionsOrder) )

        UserClickedToggleFeedback ->
            ( { model | cuLvl2 = Logic.toggle model.cuLvl2 }, Cmd.none )

        UserClickedRadioButton newChoice ->
            ( { model | cuLvl2 = Logic.update { prevState | userAnswer = newChoice } model.cuLvl2 }, Cmd.none )

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
            ( { model | cuLvl2 = Logic.update { prevState | step = decrement prevState.step } model.cuLvl2 }
            , if prevState.step /= Listening 0 then
                Ports.playAudio url

              else
                Delay.after 0 UserClickedStartAnswering
            )

        RuntimeShuffledOptionsOrder ls ->
            ( { model | optionsOrder = ls }, Cmd.none )

        UserClickedStartTraining ->
            ( { model | cuLvl2 = Logic.startTraining model.cuLvl2 }, Cmd.none )

        UserClickedStartAnswering ->
            ( { model | cuLvl2 = Logic.update { prevState | step = Answering } model.cuLvl2 }, Cmd.none )


type Step
    = Listening Ntimes
    | Answering


type alias Ntimes =
    Int


decrement : Step -> Step
decrement step =
    case step of
        Listening nTimes ->
            Listening (nTimes - 1)

        _ ->
            Answering


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
    | UserClickedStartAnswering


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
                |> optional "isTraining" Decode.bool False
    in
    Data.decodeRecords decoder


initState : State
initState =
    State "" (Listening 3)


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
    { userAnswer : String
    , step : Step
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
