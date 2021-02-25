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
import String.Interpolate exposing (interpolate)
import View



--view : { a | task : Logic.Task t s, optionsOrder : Maybe (List comparable), nextTrialMsg : f, userClickedAudio : String -> f, radioMsg : String -> f, toggleFeedbackMsg : f, startMainMsg : List t -> f, inputChangedMsg : String -> f } -> Html msg


view :
    { task : Logic.Task Trial State
    , infos : Maybe ExperimentInfo.Task
    , nextTrialMsg : msg
    , userClickedAudio : String -> msg
    , startMainMsg : List Trial -> Task -> msg
    , userToggledElementOfEntry : String -> msg
    }
    -> Html msg
view task =
    case task.task of
        Logic.NotStarted ->
            div [] [ text "experiment did not start yet" ]

        Logic.Err reason ->
            div [] [ text reason ]

        Logic.Loading ->
            div [] [ text "Loading..." ]

        Logic.Intr data ->
            case data.current of
                Just trial ->
                    let
                        viewEntry key val =
                            if Dict.get key data.state.toggledEntries |> Maybe.withDefault False then
                                text val

                            else
                                text ""

                        arrow key =
                            if Dict.get key data.state.toggledEntries |> Maybe.withDefault False then
                                fromUnstyled <| Icons.chevronDown

                            else
                                fromUnstyled <| Icons.chevronRight

                        entries =
                            [ ( "Définition: ", trial.definition )
                            , ( "Example: ", trial.example )
                            , ( "1. Translation: ", trial.translation1 )
                            , ( "2. Translation: ", trial.translation2 )
                            ]
                                |> List.map
                                    (\( key, val ) ->
                                        p [ class "flex flex-col", Html.Styled.Events.onClick (task.userToggledElementOfEntry key) ]
                                            [ span [ class "cursor-pointer flex flex-row" ] [ div [ class "h-4 w-4" ] [ arrow key ], text key ]
                                            , span [ class "p-2" ] [ viewEntry key val ]
                                            ]
                                    )
                                |> List.intersperse sep
                    in
                    div []
                        [ View.viewTraining data.infos.instructions
                            [ div [ class "container items-center justify-center w-full flex flex-col" ]
                                [ div [ class "pb-4 text-3xl font-bold flex flex-row" ]
                                    [ text trial.text
                                    , div
                                        [ class "h-6 w-6"
                                        , Html.Styled.Events.onClick (task.userClickedAudio trial.audio.url)
                                        ]
                                        [ fromUnstyled <| Icons.music ]
                                    ]
                                , div [] entries
                                , div [ class "col-start-2 col-span-4" ]
                                    [ View.button
                                        { message = task.nextTrialMsg
                                        , isDisabled = False
                                        , txt = "Next Item"
                                        }
                                    ]
                                ]
                            ]
                        ]

                Nothing ->
                    div []
                        [ text "Intro is over"
                        , View.button
                            { message = task.startMainMsg data.mainTrials data.infos
                            , isDisabled = False
                            , txt = "Start"
                            }
                        ]

        Logic.Main data ->
            case data.current of
                Just trial ->
                    let
                        viewEntry key val =
                            if Dict.get key data.state.toggledEntries |> Maybe.withDefault False then
                                text val

                            else
                                text ""

                        arrow key =
                            if Dict.get key data.state.toggledEntries |> Maybe.withDefault False then
                                fromUnstyled <| Icons.chevronDown

                            else
                                fromUnstyled <| Icons.chevronRight

                        entries =
                            [ ( "Définition: ", trial.definition )
                            , ( "Example: ", trial.example )
                            , ( "1. Translation: ", trial.translation1 )
                            , ( "2. Translation: ", trial.translation2 )
                            ]
                                |> List.map
                                    (\( key, val ) ->
                                        p [ class "flex flex-col", Html.Styled.Events.onClick (task.userToggledElementOfEntry key) ]
                                            [ span [ class "cursor-pointer flex flex-row" ] [ div [ class "h-4 w-4" ] [ arrow key ], text key ]
                                            , span [ class "p-2" ] [ viewEntry key val ]
                                            ]
                                    )
                                |> List.intersperse sep
                    in
                    div [ class "flex flex-col" ]
                        [ div [ class "pb-4 text-3xl font-bold flex flex-row" ]
                            [ text trial.text
                            , div
                                [ class "h-6 w-6"
                                , Html.Styled.Events.onClick (task.userClickedAudio trial.audio.url)
                                ]
                                [ fromUnstyled <| Icons.music ]
                            ]
                        , div [] entries
                        , div [ class "col-start-2 col-span-4" ]
                            [ View.button
                                { message = task.nextTrialMsg
                                , isDisabled = False
                                , txt = "Next Item"
                                }
                            ]
                        ]

                Nothing ->
                    text data.infos.end


type Msg
    = UserClickedNextTrial
    | UserClickedStartIntro (List Trial)
    | UserClickedStartMain (List Trial) ExperimentInfo.Task
    | UserToggleElementOfEntry String


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
            [ ( "Définition: ", False )
            , ( "Example: ", False )
            , ( "1. Translation: ", False )
            , ( "2. Translation: ", False )
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


taskId =
    "rec8eKMwCMFFtKVKD"
