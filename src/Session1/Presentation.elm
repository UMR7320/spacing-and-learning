module Session1.Presentation exposing (..)

import Data
import Dict exposing (Dict)
import ExperimentInfo exposing (Task)
import Html.Styled as Html exposing (Html, div, fromUnstyled, p, span, text)
import Html.Styled.Attributes exposing (class)
import Html.Styled.Events
import Http exposing (Error)
import Icons
import Json.Decode as Decode exposing (Decoder, string)
import Json.Decode.Pipeline exposing (..)
import Logic
import Progressbar exposing (progressBar)
import Session3.Synonym exposing (Msg(..))
import String.Interpolate exposing (interpolate)
import Task
import View


type Entry
    = Definition
    | Example
    | Translation


viewEntry : String -> { txt : String, elements : List String } -> Dict String Bool -> Html msg
viewEntry key { txt, elements } toggledEntries =
    if Dict.get key toggledEntries |> Maybe.withDefault False then
        elements |> List.map (\el -> div [ class "p-4" ] [ text el ]) |> div [ class "flex flex-col" ]

    else
        [] |> List.map text |> div []



--entries : List String -> List String -> List String -> List ( String, { txt : String, elements : List String } )


entries : List String -> List String -> List String -> (String -> msg) -> Dict String Bool -> List (Html msg)
entries d e t msg toggledEntries =
    let
        arrow key =
            if Dict.get key toggledEntries |> Maybe.withDefault False then
                span [ class "text-lg font-bold" ] [ text "⌄" ]

            else
                span [ class "text-lg font-bold" ] [ text "›" ]
    in
    [ ( "definition"
      , { txt = "Definition: "
        , elements = d
        }
      )
    , ( "example", { txt = "Example: ", elements = e } )
    , ( "translation", { txt = "Translation: ", elements = List.filter ((/=) "missing") t } )
    ]
        |> List.map
            (\( key, { txt, elements } as val ) ->
                p [ class "flex flex-col", Html.Styled.Events.onClick (msg key) ]
                    [ div [ class "text-lg hover:underline cursor-pointer" ] [ arrow key, span [ class "pl-2" ] [ text txt ] ]
                    , span [ class "p-2" ] [ viewEntry key val toggledEntries ]
                    ]
            )
        |> List.intersperse sep


view :
    { task : Logic.Task Trial State
    , infos : Maybe ExperimentInfo.Task
    , nextTrialMsg : msg
    , userClickedAudio : String -> msg
    , startMainMsg : List Trial -> Task -> msg
    , userToggledElementOfEntry : String -> msg
    , saveDataMsg : msg
    }
    -> Html msg
view task =
    case task.task of
        Logic.NotStarted ->
            div [] [ text "experiment did not start yet" ]

        Logic.Err reason ->
            div [] [ text reason ]

        Logic.Running Logic.Instructions data ->
            div [] []

        Logic.Loading ->
            div [] [ text "Loading..." ]

        Logic.Running Logic.Training data ->
            case data.current of
                Just trial ->
                    View.viewTraining data.infos.instructions
                        [ div [ class <| "pb-4 pt-4 text-3xl font-bold flex flex-row" ]
                            [ text trial.text
                            ]
                        , View.audioButton task.userClickedAudio trial.audio.url "Pronunciation"
                        , div [ class "w-56 pt-8" ] <| entries [ trial.definition ] [ trial.example ] [ trial.translation1, trial.translation2 ] task.userToggledElementOfEntry data.state.toggledEntries
                        , div [ class "pb-8" ]
                            [ View.button
                                { message = task.nextTrialMsg
                                , isDisabled = False
                                , txt = "Next Item"
                                }
                            ]
                        ]

                Nothing ->
                    View.introToMain (task.startMainMsg data.mainTrials data.infos)

        Logic.Running Logic.Main data ->
            case data.current of
                Just trial ->
                    div [ class "flex flex-col items-center" ]
                        [ progressBar data.history data.mainTrials
                        , div [ class "pb-4 text-3xl font-bold flex flex-row" ]
                            [ text trial.text
                            ]
                        , div [ class "w-1/3" ] <|
                            View.audioButton task.userClickedAudio trial.audio.url "Pronunciation"
                                :: entries [ trial.definition ] [ trial.example ] [ trial.translation1, trial.translation2 ] task.userToggledElementOfEntry data.state.toggledEntries
                        , div [ class "" ]
                            [ View.button
                                { message = task.nextTrialMsg
                                , isDisabled = False
                                , txt = "Next Item"
                                }
                            ]
                        ]

                Nothing ->
                    View.end data.infos.end task.saveDataMsg "meaning"


type Msg
    = UserClickedNextTrial
    | UserClickedStartIntro (List Trial)
    | UserClickedStartMain (List Trial) ExperimentInfo.Task
    | UserToggleElementOfEntry String


sep : Html msg
sep =
    div [ class "w-32 h-1 mt-4 mb-4" ] []


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
                |> optional "Translation_2" string "missing"
                |> required "Word_Audio" Data.decodeAudioFiles
                |> Data.decodeBool "isTraining"
    in
    Data.decodeRecords decoder


getRecords : Task.Task Error (List Trial)
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
    State "DefaultUid"
        (Dict.fromList
            [ ( "definition", False )
            , ( "example", False )
            , ( "translation", False )
            ]
        )


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
    , toggledEntries : Dict String Bool
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


taskId : String
taskId =
    "rec8eKMwCMFFtKVKD"
