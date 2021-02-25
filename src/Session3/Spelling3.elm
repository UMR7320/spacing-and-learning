module Session3.Spelling3 exposing (..)

import Data
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


view : Logic.Task Trial State -> { e | userClickedAudio : String -> msg, toggleFeedback : msg, nextTrialMsg : msg, startMainMsg : List Trial -> ExperimentInfo.Task -> msg, userChangedInput : String -> msg } -> Html msg
view exp { userClickedAudio, toggleFeedback, nextTrialMsg, startMainMsg, userChangedInput } =
    case exp of
        Logic.NotStarted ->
            div [] [ text "experiment did not start yet" ]

        Logic.Intr ({ trainingTrials, mainTrials, current, state, feedback, history } as data) ->
            case current of
                Just trial ->
                    div []
                        [ View.viewTraining data.infos.instructions
                            [ div [ class "col-start-2 col-span-4" ] [ View.trainingWheelsGeneric (List.length history) data.infos.trainingWheel [] ]
                            , div [ class "h-12 w-12 col-start-2 col-span-4", Html.Styled.Events.onClick (userClickedAudio trial.audioSentence.url) ] [ fromUnstyled <| Icons.music ]
                            , div [ class "col-start-2 col-span-4" ]
                                [ View.floatingLabel "" state.userAnswer userChangedInput feedback
                                ]
                            , div [ class "col-start-2 col-span-4" ] <|
                                [ View.genericSingleChoiceFeedback
                                    { isVisible = feedback
                                    , feedback_Correct = ( data.infos.feedback_correct, [ trial.writtenWord ] )
                                    , feedback_Incorrect = ( data.infos.feedback_incorrect, [ trial.writtenWord ] )
                                    , userAnswer = state.userAnswer |> String.trim |> String.toLower
                                    , target = trial.writtenWord
                                    , button = View.navigationButton toggleFeedback nextTrialMsg feedback
                                    }
                                ]
                            ]
                        ]

                Nothing ->
                    View.introToMain (startMainMsg mainTrials data.infos)

        Logic.Main ({ mainTrials, current, state, feedback, history } as data) ->
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
                    div [] [ h1 [] [ text "Congrats 🎉️" ], p [] [ text data.infos.end ] ]

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
    "recf5HANE632FLKbc"


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
