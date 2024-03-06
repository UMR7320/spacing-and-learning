module Session1.Presentation exposing (..)

import Activity exposing (Activity)
import ActivityInfo exposing (ActivityInfo, Session(..))
import Data
import Dict exposing (Dict)
import Html.Styled exposing (Html, div, li, span, text, ul)
import Html.Styled.Attributes exposing (class)
import Html.Styled.Events
import Http
import Json.Decode as Decode exposing (Decoder, bool, string)
import Json.Decode.Pipeline exposing (..)
import Ports
import Random
import Random.List exposing (shuffle)
import RemoteData exposing (RemoteData)
import Set exposing (Set)
import Task
import Time
import Url.Builder
import View



-- MODEL


type Entry
    = Translation


type alias Presentation =
    Activity Trial State


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
    , clickedEntries : Set String
    }


initialState : State
initialState =
    { uid = ""
    , toggledEntries = Dict.empty
    , clickedEntries = Set.empty
    }


infoLoaded : List ActivityInfo -> Presentation -> Presentation
infoLoaded infos =
    Activity.infoLoaded
        Session1
        "Words to learn"
        infos
        initState


init : String -> Model a -> ( Model a, Cmd Msg )
init group model =
    ( model
    , Cmd.batch
        [ getRecords group
        , Ports.disableAlertOnExit ()
        ]
    )



-- VIEW


viewEntry : String -> { txt : String, elements : List String } -> Dict String Bool -> Html msg
viewEntry key { elements } toggledEntries =
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


view : Activity Trial State -> Html Msg
view task =
    case task of
        Activity.NotStarted ->
            div [] [ text "experiment did not start yet" ]

        Activity.Err reason ->
            div [] [ text reason ]

        Activity.Running Activity.Instructions data ->
            div [] [ View.instructions data.infos UserClickedStartTraining ]

        Activity.Loading _ _ ->
            View.loading

        Activity.Running Activity.Training data ->
            viewTrialOrEnd data (View.introToMain (UserClickedStartMain data.mainTrials data.infos))

        Activity.Running Activity.Main data ->
            viewTrialOrEnd data (View.end data.infos.end NoOp (Just "meaning"))


viewTrialOrEnd : Activity.Data Trial State -> Html Msg -> Html Msg
viewTrialOrEnd data endView =
    case data.current of
        Just trial ->
            viewTrial trial data

        Nothing ->
            endView


viewTrial : Trial -> Activity.Data Trial State -> Html Msg
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



-- UPDATE


type Msg
    = GotTrials (RemoteData Http.Error (List Trial))
    | GotRandomizedTrials (List Trial)
    | UserClickedNextTrial
    | NextTrial Time.Posix
    | UserClickedStartMain (List Trial) ActivityInfo
    | UserToggleElementOfEntry String
    | UserClickedStartAudio String
    | UserClickedStartTraining
    | NoOp


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
    { superModel | presentation : Activity Trial State }


update : Msg -> Model superModel -> ( Model superModel, Cmd Msg )
update msg model =
    case msg of
        GotTrials (RemoteData.Success trials) ->
            ( model
            , Random.generate GotRandomizedTrials (shuffle trials)
            )

        GotRandomizedTrials trials ->
            ( { model | presentation = Activity.trialsLoaded trials initialState model.presentation }
            , Cmd.none
            )

        GotTrials (RemoteData.Failure error) ->
            ( { model
                | presentation = Activity.Err (Data.buildErrorMessage error)
              }
            , Cmd.none
            )

        GotTrials _ ->
            ( model, Cmd.none )

        UserClickedNextTrial ->
            ( model, Task.perform NextTrial Time.now )

        NextTrial timestamp ->
            ( { model | presentation = Activity.next timestamp initState model.presentation }, Cmd.none )

        UserClickedStartMain _ _ ->
            ( { model | presentation = Activity.startMain model.presentation initState }, Cmd.none )

        UserToggleElementOfEntry id ->
            let
                prevState =
                    Activity.getState model.presentation

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
                            Activity.update
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
                    Activity.getState model.presentation
            in
            case prevState of
                Nothing ->
                    ( model, Cmd.none )

                Just state ->
                    ( { model | presentation = Activity.update { state | clickedEntries = Set.insert "audio" state.clickedEntries } model.presentation }, Ports.playAudio url )

        UserClickedStartTraining ->
            ( { model | presentation = Activity.startTraining model.presentation }, Cmd.none )

        NoOp ->
            ( model, Cmd.none )



-- HTTP


decodePresentationInput : Decoder (List Trial)
decodePresentationInput =
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


getRecords : String -> Cmd Msg
getRecords group =
    Http.get
        { url =
            Url.Builder.absolute [ ".netlify", "functions", "api" ]
                [ Url.Builder.string "base" "input"
                , Url.Builder.string "view" "Presentation"
                , Url.Builder.string "filterByFormula" ("{Classe} = \"" ++ group ++ "\"")
                ]
        , expect = Http.expectJson (RemoteData.fromResult >> GotTrials) decodePresentationInput
        }
