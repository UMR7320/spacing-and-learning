module Session1.Presentation exposing (..)

import Data
import Dict exposing (Dict)
import ExperimentInfo
import Html.Styled exposing (Html, div, li, p, span, text, ul)
import Html.Styled.Attributes exposing (class)
import Html.Styled.Events
import Http exposing (Error)
import Json.Decode as Decode exposing (Decoder, bool, string)
import Json.Decode.Pipeline exposing (..)
import Logic
import Ports
import Progressbar exposing (progressBar)
import Session3.Synonym exposing (Msg(..))
import Set
import Task
import View


type Entry
    = Translation


type alias Presentation =
    Logic.Task Trial State


viewEntry : String -> { txt : String, elements : List String } -> Dict String Bool -> Html msg
viewEntry key { txt, elements } toggledEntries =
    if Dict.get key toggledEntries |> Maybe.withDefault False then
        elements |> List.map (\el -> li [] [ text el ]) |> ul []

    else
        [] |> List.map text |> div []


entries : List String -> List String -> List String -> (String -> msg) -> Dict String Bool -> List (Html msg)
entries d e t msg toggledEntries =
    let
        arrow key =
            if Dict.get key toggledEntries |> Maybe.withDefault False then
                span [ class "font-bold" ] [ text "⌄" ]

            else
                span [ class "font-bold" ] [ text "›" ]
    in
    [ ( "definition"
      , { txt = "Definition "
        , elements = d
        }
      )
    , ( "example", { txt = "Example ", elements = e } )
    , ( "translation", { txt = "Translation ", elements = List.filter ((/=) "missing") t } )
    ]
        |> List.map
            (\( key, { txt } as val ) ->
                div [ class "summary", Html.Styled.Events.onClick (msg key) ]
                    [ div [ class "hover:underline cursor-pointer bg-green-500 font-bold text-white p-4 rounded-lg" ] [ arrow key, span [ class "pl-2" ] [ text txt ] ]
                    , div [ class "p-2 details" ] [ viewEntry key val toggledEntries ]
                    ]
            )


view : Logic.Task Trial State -> Html Msg
view task =
    case task of
        Logic.NotStarted ->
            div [] [ text "experiment did not start yet" ]

        Logic.Err reason ->
            div [] [ text reason ]

        Logic.Running Logic.Instructions data ->
            div [] [ View.instructions data.infos UserClickedStartTraining ]

        Logic.Loading ->
            View.loading

        Logic.Running Logic.Training data ->
            viewTrialOrEnd data (View.introToMain (UserClickedStartMain data.mainTrials data.infos))

        Logic.Running Logic.Main data ->
            viewTrialOrEnd data (View.end data.infos.end NoOp "meaning")


viewTrialOrEnd data endView =
    case data.current of
        Just trial ->
            viewTrial trial data

        Nothing ->
            endView


viewTrial trial data =
    div [ class "full-bleed flow" ]
        [ div
            [ class "word-presentation" ]
            ([ div [ class "text-3xl text-center italic font-bold" ] [ text ("to " ++ trial.text) ]
             , View.audioButton UserClickedStartAudio trial.audio.url "Pronunciation"
             ]
                ++ entries [ trial.definition ] [ trial.example ] [ trial.translation1, trial.translation2 ] UserToggleElementOfEntry data.state.toggledEntries
            )
        , View.button
            { message = UserClickedNextTrial
            , isDisabled =
                data.state.clickedEntries /= Set.fromList [ "definition", "example", "translation", "audio" ]
            , txt = "Continue"
            }
        ]


type Msg
    = UserClickedNextTrial
    | UserClickedStartMain (List Trial) ExperimentInfo.Task
    | UserToggleElementOfEntry String
    | UserClickedStartAudio String
    | UserClickedStartTraining
    | NoOp


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
                |> optional "isTraining" bool False
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
        Set.empty


type alias Model superModel =
    { superModel | presentation : Logic.Task Trial State }


update : Msg -> Model superModel -> ( Model superModel, Cmd Msg )
update msg model =
    case msg of
        UserClickedNextTrial ->
            ( { model | presentation = Logic.next initState model.presentation }, Cmd.none )

        UserClickedStartMain _ _ ->
            ( { model | presentation = Logic.startMain model.presentation initState }, Cmd.none )

        UserToggleElementOfEntry id ->
            let
                prevState =
                    Logic.getState model.presentation

                updateEntry state =
                    Dict.map
                        (\k v ->
                            if k == id then
                                not v

                            else
                                False
                        )
                        state.toggledEntries
            in
            case prevState of
                Just state ->
                    ( { model
                        | presentation =
                            Logic.update
                                { state
                                    | toggledEntries =
                                        updateEntry state
                                    , clickedEntries = Set.insert id state.clickedEntries
                                }
                                model.presentation
                      }
                    , Cmd.none
                    )

                Nothing ->
                    ( model, Cmd.none )

        UserClickedStartAudio url ->
            let
                prevState =
                    Logic.getState model.presentation
            in
            case prevState of
                Nothing ->
                    ( model, Cmd.none )

                Just state ->
                    ( { model | presentation = Logic.update { state | clickedEntries = Set.insert "audio" state.clickedEntries } model.presentation }, Ports.playAudio url )

        UserClickedStartTraining ->
            ( { model | presentation = Logic.startTraining model.presentation }, Cmd.none )

        NoOp ->
            ( model, Cmd.none )


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
    , clickedEntries : Set.Set String
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
