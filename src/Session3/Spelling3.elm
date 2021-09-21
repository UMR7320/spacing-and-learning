module Session3.Spelling3 exposing (..)

import Data
import Delay
import Dict
import ExperimentInfo
import Html.Styled exposing (Html, div, fromUnstyled, text)
import Html.Styled.Attributes exposing (class)
import Html.Styled.Events
import Http exposing (Error)
import Icons
import Json.Decode as Decode exposing (Decoder, string)
import Json.Decode.Pipeline exposing (..)
import Logic
import Ports
import Progressbar exposing (progressBar)
import Session1.Presentation exposing (Msg(..))
import View


view :
    Logic.Task Trial State
    -> Html Msg
view exp =
    case exp of
        Logic.NotStarted ->
            div [] [ text "experiment did not start yet" ]

        Logic.Running Logic.Instructions data ->
            div [] [ View.instructions data.infos UserClickedStartTraining ]

        Logic.Running Logic.Training ({ current, state, feedback, history } as data) ->
            case ( current, state.step ) of
                ( Just trial, Listening nTimes ) ->
                    div [ class "flex flex-col items-center" ]
                        [ viewLimitedTimesAudioButton nTimes trial
                        , if nTimes < 3 then
                            View.floatingLabel "Type here" state.userAnswer UserChangedInput feedback

                          else
                            div [] []
                        , View.genericSingleChoiceFeedback
                            { isVisible = feedback
                            , feedback_Correct = ( data.infos.feedback_correct, [ trial.writtenWord ] )
                            , feedback_Incorrect = ( data.infos.feedback_incorrect, [ trial.writtenWord ] )
                            , userAnswer = state.userAnswer |> String.trim |> String.toLower
                            , target = trial.writtenWord
                            , button = View.navigationButton UserClickedToggleFeedback UserClickedNextTrial feedback data.state.userAnswer
                            }
                        ]

                ( Nothing, _ ) ->
                    View.introToMain UserClickedStartMain

                _ ->
                    div [] []

        Logic.Running Logic.Main ({ current, state, feedback } as data) ->
            case ( current, state.step ) of
                ( Just trial, Listening nTimes ) ->
                    div [ class "flex flex-col items-center " ]
                        [ View.tooltip data.infos.instructions_short
                        , progressBar data.history data.mainTrials
                        , viewLimitedTimesAudioButton nTimes trial
                        , if nTimes < 3 then
                            View.floatingLabel "Type here" state.userAnswer UserChangedInput feedback

                          else
                            div [] []
                        , View.genericSingleChoiceFeedback
                            { isVisible = feedback
                            , feedback_Correct = ( data.infos.feedback_correct, [ trial.writtenWord ] )
                            , feedback_Incorrect = ( data.infos.feedback_incorrect, [ trial.writtenWord ] )
                            , userAnswer = state.userAnswer |> String.trim |> String.toLower
                            , target = trial.writtenWord
                            , button = View.navigationButton UserClickedToggleFeedback UserClickedNextTrial feedback data.state.userAnswer
                            }
                        ]

                ( Nothing, _ ) ->
                    View.end data.infos.end UserClickedSaveData "context-understanding"

                _ ->
                    div [] []

        Logic.Err reason ->
            div [] [ text <| "I stumbled into an error : " ++ reason ]

        Logic.Loading ->
            View.loading


type Msg
    = UserClickedNextTrial
    | UserClickedToggleFeedback
    | UserClickedStartTraining
    | UserClickedStartMain
    | UserChangedInput String
    | UserClickedSaveData
    | ServerRespondedWithLastRecords (Result Http.Error (List ()))
    | UserClickedPlayAudio String
    | UserClickedStartAnswering


getTrialsFromServer : (Result Error (List Trial) -> msg) -> Cmd msg
getTrialsFromServer msgHandler =
    Data.getTrialsFromServer_ "input" "ContextUnderstandingLvl3" msgHandler decodeTranslationInput


update msg model =
    let
        prevState =
            Logic.getState model.spelling3 |> Maybe.withDefault initState
    in
    case msg of
        UserClickedNextTrial ->
            ( { model | spelling3 = Logic.next initState model.spelling3 }, Cmd.none )

        UserClickedToggleFeedback ->
            ( { model | spelling3 = Logic.toggle model.spelling3 }, Cmd.none )

        UserClickedStartTraining ->
            ( { model | spelling3 = Logic.startTraining model.spelling3 }, Cmd.none )

        UserClickedStartMain ->
            ( { model | spelling3 = Logic.startMain model.spelling3 initState }, Cmd.none )

        UserChangedInput new ->
            ( { model | spelling3 = Logic.update { prevState | userAnswer = new } model.spelling3 }, Cmd.none )

        UserClickedSaveData ->
            let
                responseHandler =
                    ServerRespondedWithLastRecords
            in
            ( model, Logic.saveData responseHandler model.user taskId model.scrabbleTask )

        ServerRespondedWithLastRecords _ ->
            ( model, Cmd.none )

        UserClickedPlayAudio url ->
            ( { model | spelling3 = Logic.update { prevState | step = decrement prevState.step } model.spelling3 }
            , if prevState.step /= Listening 0 then
                Ports.playAudio url

              else
                Delay.after 0 UserClickedStartAnswering
            )

        UserClickedStartAnswering ->
            ( { model | spelling3 = Logic.update { prevState | step = Answering } model.spelling3 }, Cmd.none )


decodeTranslationInput : Decoder (List Trial)
decodeTranslationInput =
    let
        decoder =
            Decode.succeed Trial
                |> required "UID" string
                |> required "Word_Text" string
                |> optional "Word_Audio" Data.decodeAudioFiles (Data.AudioFile "" "")
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
    , isTraining = False
    }


type alias Trial =
    { uid : String
    , writtenWord : String
    , audioSentence : Data.AudioFile
    , isTraining : Bool
    }


type alias State =
    { uid : String
    , userAnswer : String
    , step : Step
    }


type Step
    = Listening Int
    | Answering


decrement : Step -> Step
decrement step =
    case step of
        Listening nTimes ->
            Listening (nTimes - 1)

        _ ->
            Answering


taskId =
    "recJucOXEZzJj6Uui"


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


viewLimitedTimesAudioButton nTimes trial =
    if nTimes == 3 then
        View.audioButton UserClickedPlayAudio trial.audioSentence.url "Listen"

    else if nTimes == 2 then
        View.audioButton UserClickedPlayAudio trial.audioSentence.url "Listen again?"

    else if nTimes == 1 then
        View.audioButton UserClickedPlayAudio trial.audioSentence.url "Listen for the last time?"

    else
        View.button { isDisabled = nTimes == 0, message = UserClickedStartAnswering, txt = "What happened?" }
