module Experiment.Presentation exposing (..)

import Data
import ExperimentInfo
import Html.Styled as Html exposing (Html, div, fromUnstyled, p, span, text)
import Html.Styled.Attributes exposing (class)
import Html.Styled.Events
import Http exposing (Error)
import Icons
import Json.Decode as Decode exposing (Decoder, string)
import Json.Decode.Pipeline exposing (..)
import PsychTask
import String.Interpolate exposing (interpolate)
import View



--view : { a | task : PsychTask.Task t s, optionsOrder : Maybe (List comparable), nextTrialMsg : f, userClickedAudio : String -> f, radioMsg : String -> f, toggleFeedbackMsg : f, startMainMsg : List t -> f, inputChangedMsg : String -> f } -> Html msg


view :
    { task : PsychTask.Task Trial State
    , infos : Maybe ExperimentInfo.Task
    , nextTrialMsg : msg
    , userClickedAudio : String -> msg
    , startMainMsg : List Trial -> msg
    }
    -> Html msg
view task =
    case ( task.task, task.infos ) of
        ( _, Nothing ) ->
            div [] [ text "Error : I can't find the task's infos" ]

        ( PsychTask.NotStartedYet, Just info ) ->
            div [] [ text "experiment did not start yet" ]

        ( PsychTask.Intr data, Just info ) ->
            case data.current of
                Just trial ->
                    div []
                        [ View.viewTraining info.instructions
                            [ p [ class "col-start-2 col-span-4" ] []
                            , div [ class "p-2 col-start-2 col-span-4" ] []
                            , p [ class "col-start-2 col-span-4" ] []
                            , div [ class "col-start-2 col-span-4" ]
                                [ View.button
                                    { message = task.nextTrialMsg
                                    , isDisabled = False
                                    , txt = "Next Training Item"
                                    }
                                ]
                            ]
                        ]

                Nothing ->
                    div []
                        [ text "Intro is over"
                        , View.button
                            { message = task.startMainMsg data.mainTrials
                            , isDisabled = False
                            , txt = "Start"
                            }
                        ]

        ( PsychTask.IntroOver, Just info ) ->
            text "L'entrainement est fini"

        ( PsychTask.Main data, Just info ) ->
            case data.current of
                Just trial ->
                    div [ class "flex flex-col" ]
                        [ div [ class "pb-4 text-3xl font-bold flex flex-row" ]
                            [ text trial.text
                            , div
                                [ class "h-6 w-6"
                                , Html.Styled.Events.onClick (task.userClickedAudio trial.audio.url)
                                ]
                                [ fromUnstyled <| Icons.music ]
                            ]
                        , p [ class "flex flex-col" ] [ span [ class "italic" ] [ text "Definition: " ], span [ class "p-2" ] [ text trial.definition ] ]
                        , sep
                        , p [ class "flex flex-col" ] [ span [ class "italic" ] [ text "Example: " ], span [ class "p-2" ] [ text trial.example ] ]
                        , sep
                        , p [ class "flex flex-col" ] [ span [ class "italic" ] [ text "1. Translation: " ], span [ class "p-2" ] [ text trial.translation1 ] ]
                        , p [ class "flex flex-col" ] [ span [ class "italic" ] [ text "2. Translation: " ], span [ class "p-2" ] [ text trial.translation2 ] ]
                        , div [ class "col-start-2 col-span-4" ]
                            [ View.button
                                { message = task.nextTrialMsg
                                , isDisabled = False
                                , txt = "Next Item"
                                }
                            ]
                        ]

                Nothing ->
                    text info.end

        ( PsychTask.Over, Just info ) ->
            text "I'm over"


type Msg
    = UserClickedNextTrial
    | ServerRespondedWith (Result Http.Error (List Trial))
    | UserClickedStartIntro (List Trial)
    | UserClickedStartMain (List Trial)


sep =
    div [ class "w-32 h-1 bg-gray-300 mt-4 mb-4" ] []


getTrialsFromServer : (Result Error (List Trial) -> msg) -> Cmd msg
getTrialsFromServer msgHandler =
    Data.getTrialsFromServer_ "input" "Presentation" msgHandler decodeTranslationInput


decodeTranslationInput : Decoder (List Trial)
decodeTranslationInput =
    let
        decoder =
            Decode.succeed Trial
                |> required "UID" string
                |> required "Word_Text" string
                |> required "Definition" string
                |> required "Example" string
                |> required "Translation_1" string
                |> optional "Translation_2" string "Missing translation 2"
                |> required "Word_Audio" Data.decodeAudioFiles
                |> Data.decodeBool "isTraining"
    in
    Data.decodeRecords decoder


initState : State
initState =
    State "DefaultUid" ""


defaultTrial : Trial
defaultTrial =
    { uid = "String"
    , text = "String"
    , definition = "String"
    , example = "String"
    , translation1 = "String"
    , translation2 = "String"
    , audio = Data.AudioFile "" ""
    , isTraining = False
    }


type alias Trial =
    { uid : String
    , text : String
    , definition : String
    , example : String
    , translation1 : String
    , translation2 : String
    , audio : Data.AudioFile
    , isTraining : Bool
    }


type alias State =
    { uid : String
    , userAnswer : String
    }
