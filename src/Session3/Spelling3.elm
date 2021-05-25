module Session3.Spelling3 exposing (..)

import Data
import Dict
import ExperimentInfo exposing (Task)
import Html.Styled as Html exposing (Html, div, fromUnstyled, h1, p, span, text)
import Html.Styled.Attributes exposing (class)
import Html.Styled.Events
import Http exposing (Error)
import Icons
import Json.Decode as Decode exposing (Decoder, string)
import Json.Decode.Pipeline exposing (..)
import Logic
import View


view :
    Logic.Task Trial State
    ->
        { e
            | userClickedAudio : String -> msg
            , toggleFeedback : msg
            , nextTrialMsg : msg
            , startMainMsg : List Trial -> ExperimentInfo.Task -> msg
            , userChangedInput : String -> msg
            , saveData : msg
        }
    -> Html msg
view exp { userClickedAudio, toggleFeedback, nextTrialMsg, saveData, startMainMsg, userChangedInput } =
    case exp of
        Logic.NotStarted ->
            div [] [ text "experiment did not start yet" ]

        Logic.Running Logic.Instructions data ->
            div [] []

        Logic.Running Logic.Training ({ trainingTrials, mainTrials, current, state, feedback, history } as data) ->
            case current of
                Just trial ->
                    div []
                        [ View.viewTraining data.infos.instructions
                            [ View.trainingWheelsGeneric (List.length history) data.infos.trainingWheel []
                            , View.audioButton userClickedAudio trial.audioSentence.url "word"
                            , div [ class "p-8" ] [ View.floatingLabel "" state.userAnswer userChangedInput feedback ]
                            , View.genericSingleChoiceFeedback
                                { isVisible = feedback
                                , feedback_Correct = ( data.infos.feedback_correct, [ trial.writtenWord ] )
                                , feedback_Incorrect = ( data.infos.feedback_incorrect, [ trial.writtenWord ] )
                                , userAnswer = state.userAnswer |> String.trim |> String.toLower
                                , target = trial.writtenWord
                                , button = View.navigationButton toggleFeedback nextTrialMsg feedback
                                }
                            ]
                        ]

                Nothing ->
                    View.introToMain (startMainMsg mainTrials data.infos)

        Logic.Running Logic.Main ({ mainTrials, current, state, feedback, history } as data) ->
            case current of
                Just trial ->
                    div [ class "container flex flex-col justify-center items-center max-w-3xl m-4 p-4" ]
                        [ div [ class "h-8 w-8 pb-16", Html.Styled.Events.onClick (userClickedAudio trial.audioSentence.url) ] [ fromUnstyled <| Icons.music ]
                        , div [ class "pb-8" ] [ View.floatingLabel "Type here" state.userAnswer userChangedInput feedback ]
                        , View.genericSingleChoiceFeedback
                            { isVisible = feedback
                            , feedback_Correct = ( data.infos.feedback_correct, [ trial.writtenWord ] )
                            , feedback_Incorrect = ( data.infos.feedback_incorrect, [ trial.writtenWord ] )
                            , userAnswer = state.userAnswer |> String.trim |> String.toLower
                            , target = trial.writtenWord
                            , button = View.navigationButton toggleFeedback nextTrialMsg feedback
                            }
                        ]

                Nothing ->
                    View.end data.infos.end saveData "context-understanding"

        Logic.Err reason ->
            div [] [ text <| "I stumbled into an error : " ++ reason ]

        Logic.Loading ->
            div [] [ text "Loading..." ]


type Msg
    = UserClickedNextTrial
    | UserClickedToggleFeedback
    | UserClickedStartIntro (List Trial)
    | UserClickedStartMain (List Trial) Task
    | UserChangedInput String
    | UserClickedSaveData
    | ServerRespondedWithLastRecords (Result Http.Error (List ()))


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
                |> optional "Word_Audio" Data.decodeAudioFiles (Data.AudioFile "" "")
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
    }


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
