module Session3.CU3 exposing (..)

import Data
import Delay
import Dict
import ExperimentInfo
import Html.Styled as Html exposing (div, span, text)
import Html.Styled.Attributes exposing (class)
import Html.Styled.Events exposing (onInput)
import Http exposing (Error)
import Json.Decode as Decode exposing (Decoder, string)
import Json.Decode.Pipeline exposing (..)
import Logic
import Ports
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
            View.loading

        Logic.Err reason ->
            div [] [ text <| "I stumbled into an error : " ++ reason ]

        Logic.Running Logic.Instructions data ->
            div [] [ View.instructions data.infos.instructions UserClickedStartTraining ]

        Logic.Running Logic.Training { current, state, feedback } ->
            case ( current, state.step ) of
                ( Just trial, Listening nTimes ) ->
                    let
                        ( context, dialog ) =
                            case String.split "/" trial.context of
                                x :: xs ->
                                    ( x, String.concat xs )

                                _ ->
                                    ( "Missing context", "Missing dialog" )
                    in
                    div [ class "flex flex-col items-center" ]
                        [ div [ class "text-xl" ] [ View.fromMarkdown context ]
                        , viewLimitedTimesAudioButton nTimes trial
                        , Html.pre [ class "text-lg m-4 text-center" ]
                            [ View.fromMarkdown dialog
                            ]
                        , View.textAreaWithReadonlyAmorce
                            { id_ = "production"
                            , amorce = trial.amorce
                            , isFeedback = feedback
                            , userAnswer = state.userAnswer
                            , onInputMsg = UserChangedInput
                            }
                        , View.genericNeutralFeedback
                            { isVisible = feedback
                            , feedback_Correct = ( trial.feedback, [] )
                            , button = View.navigationButton UserClickedToggleFeedback UserClickedNextTrial feedback state.userAnswer
                            }
                        ]

                ( Nothing, _ ) ->
                    View.introToMain UserClickedStartMain

        Logic.Running Logic.Main ({ mainTrials, current, state, feedback, history } as data) ->
            case ( current, state.step ) of
                ( Just trial, Listening nTimes ) ->
                    let
                        ( context, dialog ) =
                            case String.split "/" trial.context of
                                x :: xs ->
                                    ( x, String.concat xs )

                                _ ->
                                    ( "Missing context", "Missing dialog" )
                    in
                    div [ class "flex flex-col items-center" ]
                        [ View.tooltip
                            data.infos.instructions_short
                        , progressBar history mainTrials
                        , div [ class "text-xl text-center" ] [ View.fromMarkdown context ]
                        , viewLimitedTimesAudioButton nTimes trial
                        , Html.pre [ class "text-center font-bold" ]
                            [ View.fromMarkdown dialog
                            ]
                        , View.textAreaWithReadonlyAmorce
                            { id_ = "production"
                            , amorce = trial.amorce
                            , isFeedback = feedback
                            , userAnswer = state.userAnswer
                            , onInputMsg = UserChangedInput
                            }
                        , View.genericNeutralFeedback
                            { isVisible = feedback
                            , feedback_Correct = ( trial.feedback, [] )
                            , button = View.navigationButton UserClickedToggleFeedback UserClickedNextTrial feedback state.userAnswer
                            }
                        ]

                ( Nothing, _ ) ->
                    View.end data.infos.end UserClickedSaveData "../post-tests/cw"


type Msg
    = UserClickedNextTrial
    | UserClickedToggleFeedback
    | UserClickedStartMain
    | UserChangedInput String
    | UserClickedSaveData
    | ServerRespondedWithLastRecords (Result Http.Error (List ()))
    | UserClickedStartTraining
    | RuntimeShuffledOptionsOrder (List Int)
    | UserClickedAudio String
    | UserClickedStartAnswering


update msg model =
    let
        prevState =
            Logic.getState model.cu3 |> Maybe.withDefault initState
    in
    case msg of
        UserClickedNextTrial ->
            ( { model | cu3 = Logic.next initState model.cu3 }, Random.generate RuntimeShuffledOptionsOrder (Random.List.shuffle model.optionsOrder) )

        UserClickedToggleFeedback ->
            ( { model | cu3 = Logic.toggle model.cu3 }, Cmd.none )

        UserClickedStartMain ->
            ( { model | cu3 = Logic.startMain model.cu3 initState }, Cmd.none )

        UserChangedInput new ->
            ( { model | cu3 = Logic.update { prevState | userAnswer = new } model.cu3 }, Cmd.none )

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

        UserClickedAudio url ->
            ( { model | cu3 = Logic.update { prevState | step = decrement prevState.step } model.cu3 }
            , if prevState.step /= Listening 0 then
                Ports.playAudio url

              else
                Delay.after 0 UserClickedStartAnswering
            )

        UserClickedStartAnswering ->
            ( model, Cmd.none )



--( { model | cu3 = Logic.update { prevState | step = Answering } model.cu3 }, Cmd.none )


viewLimitedTimesAudioButton nTimes trial =
    if nTimes == 3 then
        View.audioButton UserClickedAudio trial.audioSentence.url "Listen"

    else if nTimes == 2 then
        View.audioButton UserClickedAudio trial.audioSentence.url "Listen again?"

    else if nTimes == 1 then
        View.audioButton UserClickedAudio trial.audioSentence.url "Listen for the last time?"

    else
        View.button { isDisabled = nTimes == 0, message = UserClickedStartAnswering, txt = "What happened?" }


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
                |> optional "isTraining" Decode.bool False
    in
    Data.decodeRecords decoder


initState : State
initState =
    State "DefaultUid" "" (Listening 3)


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
    , step : Step
    }


type Step
    = Listening Int


decrement : Step -> Step
decrement step =
    case step of
        Listening nTimes ->
            Listening (nTimes - 1)


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
