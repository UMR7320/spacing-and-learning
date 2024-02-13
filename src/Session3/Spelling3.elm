module Session3.Spelling3 exposing (..)

import Data
import Delay
import ActivityInfo exposing (Session(..))
import Html.Styled exposing (Html, div, input, label, text)
import Html.Styled.Attributes exposing (class, for, id, value)
import Html.Styled.Events exposing (onInput)
import Http exposing (Error)
import Json.Decode as Decode exposing (Decoder, string)
import Json.Decode.Pipeline exposing (..)
import Json.Encode as Encode
import Activity exposing (Activity)
import Ports
import Task
import Time
import View
import ActivityInfo exposing (ActivityInfo)



-- MODEL


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


type alias Spelling3 =
    Activity Trial State


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


start : List ActivityInfo -> List Trial -> Activity Trial State
start infos trials =
    Activity.startIntro
        (ActivityInfo.activityInfo infos Session3 "Spelling 3")
        (List.filter (\datum -> datum.isTraining) trials)
        (List.filter (\datum -> not datum.isTraining) trials)
        initState


infoLoaded : List ActivityInfo -> Spelling3 -> Spelling3
infoLoaded infos =
    Activity.infoLoaded
        Session3
        "Spelling 3"
        infos
        initState


--VIEW


view : Activity Trial State -> Html Msg
view exp =
    case exp of
        Activity.NotStarted ->
            div [] [ text "experiment did not start yet" ]

        Activity.Running Activity.Instructions data ->
            div [] [ View.instructions data.infos UserClickedStartTraining ]

        Activity.Running Activity.Training ({ current, state, feedback, history } as data) ->
            case ( current, state.step ) of
                ( Just trial, Listening nTimes ) ->
                    div [ class "flex flex-col items-center flow" ]
                        [ viewLimitedTimesAudioButton nTimes trial
                        , if nTimes < 3 then
                            div []
                                [ label [ for "answer" ] [ text "Type your answer" ]
                                , input [ id "answer", class "text-xl", value state.userAnswer, onInput UserChangedInput ] []
                                ]

                          else
                            text ""
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

        Activity.Running Activity.Main ({ current, state, feedback } as data) ->
            case ( current, state.step ) of
                ( Just trial, Listening nTimes ) ->
                    div [ class "flex flex-col items-center " ]
                        [ viewLimitedTimesAudioButton nTimes trial
                        , if nTimes < 3 then
                            div []
                                [ label [ for "answer" ] [ text "Type your answer" ]
                                , input [ id "answer", class "text-xl", value state.userAnswer, onInput UserChangedInput ] []
                                ]

                          else
                            text ""
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
                    View.end data.infos.end UserClickedSaveData (Just "context-understanding")

                _ ->
                    div [] []

        Activity.Err reason ->
            div [] [ text <| "I stumbled into an error : " ++ reason ]

        Activity.Loading _ _ ->
            View.loading


viewLimitedTimesAudioButton nTimes trial =
    if nTimes == 3 then
        View.audioButton UserClickedPlayAudio trial.audioSentence.url "Listen"

    else if nTimes == 2 then
        View.audioButton UserClickedPlayAudio trial.audioSentence.url "Listen again?"

    else if nTimes == 1 then
        View.audioButton UserClickedPlayAudio trial.audioSentence.url "Listen for the last time?"

    else
        View.button { isDisabled = nTimes == 0, message = UserClickedStartAnswering, txt = "What happened?" }



-- UPDATE


type Msg
    = UserClickedNextTrial
    | NextTrial Time.Posix
    | UserClickedToggleFeedback
    | UserClickedStartTraining
    | UserClickedStartMain
    | UserChangedInput String
    | UserClickedSaveData
    | UserClickedPlayAudio String
    | UserClickedStartAnswering
    | HistoryWasSaved (Result Http.Error String)


update msg model =
    let
        prevState =
            Activity.getState model.spelling3 |> Maybe.withDefault initState
    in
    case msg of
        UserClickedNextTrial ->
            ( model, Task.perform NextTrial Time.now )

        NextTrial timestamp ->
            let
                newModel =
                    { model | spelling3 = Activity.next timestamp initState model.spelling3 }
            in
            ( newModel, saveData newModel )

        UserClickedToggleFeedback ->
            ( { model | spelling3 = Activity.toggle model.spelling3 }, Cmd.none )

        UserClickedStartTraining ->
            ( { model | spelling3 = Activity.startTraining model.spelling3 }, Cmd.none )

        UserClickedStartMain ->
            ( { model | spelling3 = Activity.startMain model.spelling3 initState }, Cmd.none )

        UserChangedInput new ->
            ( { model | spelling3 = Activity.update { prevState | userAnswer = new } model.spelling3 }, Cmd.none )

        -- data is now saved after each "trial", so this does nothing and shoud be removed
        UserClickedSaveData ->
            ( model, Cmd.none )

        HistoryWasSaved _ ->
            ( model, Cmd.none )

        UserClickedPlayAudio url ->
            ( { model | spelling3 = Activity.update { prevState | step = decrement prevState.step } model.spelling3 }
            , if prevState.step /= Listening 0 then
                Ports.playAudio url

              else
                Delay.after 0 UserClickedStartAnswering
            )

        UserClickedStartAnswering ->
            ( { model | spelling3 = Activity.update { prevState | step = Answering } model.spelling3 }, Cmd.none )


decrement : Step -> Step
decrement step =
    case step of
        Listening nTimes ->
            Listening (nTimes - 1)

        _ ->
            Answering



-- HTTP


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
                |> optional "isTraining" Decode.bool False
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


saveData model =
    let
        history =
            Activity.getHistory model.spelling3
                |> List.filter (\( trial, _, _ ) -> not trial.isTraining)

        userId =
            model.user |> Maybe.withDefault "recd18l2IBRQNI05y"

        payload =
            updateHistoryEncoder userId history
    in
    Http.request
        { method = "PATCH"
        , headers = []
        , url = Data.buildQuery { app = Data.apps.spacing, base = "users", view_ = "Session3_output" }
        , body = Http.jsonBody payload
        , expect = Http.expectJson HistoryWasSaved (Decode.succeed "OK")
        , timeout = Nothing
        , tracker = Nothing
        }


updateHistoryEncoder : String -> List ( Trial, State, Time.Posix ) -> Encode.Value
updateHistoryEncoder userId history =
    -- The Netflify function that receives PATCH requests only works with arrays
    Encode.list
        (\_ ->
            Encode.object
                [ ( "id", Encode.string userId )
                , ( "fields", historyEncoder userId history )
                ]
        )
        [ ( userId, history ) ]


historyEncoder : String -> List ( Trial, State, Time.Posix ) -> Encode.Value
historyEncoder userId history =
    Encode.object
        -- airtable does not support JSON columns, so we save giant JSON strings
        [ ( "Spelling3", Encode.string (Encode.encode 0 (Encode.list historyItemEncoder history)) )
        ]


historyItemEncoder : ( Trial, State, Time.Posix ) -> Encode.Value
historyItemEncoder ( { uid, writtenWord }, { userAnswer }, timestamp ) =
    Encode.object
        [ ( "trialUid", Encode.string uid )
        , ( "target", Encode.string writtenWord )
        , ( "answer", Encode.string userAnswer )
        , ( "answeredAt", Encode.int (Time.posixToMillis timestamp) )
        ]
