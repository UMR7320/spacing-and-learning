module Session1.Spelling1 exposing (..)

import Activity exposing (Activity)
import ActivityInfo exposing (ActivityInfo, Session(..))
import Data exposing (decodeRecords)
import Html.Styled exposing (Html, div, text)
import Html.Styled.Attributes exposing (class, disabled)
import Http
import Json.Decode as Decode
import Json.Decode.Pipeline exposing (..)
import Json.Encode as Encode
import Ports
import Random
import Random.List exposing (shuffle)
import RemoteData exposing (RemoteData)
import Task
import Time
import Url.Builder
import View



-- MODEL


type alias State =
    { inputUid : String
    , userAnswer : String
    , remainingListenings : Int
    , step : Step
    }


type Step
    = ListeningFirstTime
    | Answering


type alias Spelling1 =
    Activity Trial State


type alias Trial =
    { uid : String
    , target : String
    , distractor1 : String
    , distractor2 : String
    , distractor3 : String
    , isTraining : Bool
    , audio : Data.AudioFile
    }


type alias Model superModel =
    { superModel
        | spelling1 : Activity Trial State
        , user : Maybe String
        , optionsOrder : List Int
    }


initState : State
initState =
    State "DefaultTrialUID" "" 3 ListeningFirstTime


infoLoaded : List ActivityInfo -> Spelling1 -> Spelling1
infoLoaded infos =
    Activity.infoLoaded
        Session1
        "Spelling 1"
        infos
        initState


init : String -> Model a -> ( Model a, Cmd Msg )
init group model =
    ( { model | spelling1 = Activity.loading model.spelling1 }
    , Cmd.batch
        [ getRecords group
        , Ports.enableAlertOnExit ()
        ]
    )



-- VIEW


viewActivity : Activity.Data Trial State -> Trial -> List Int -> List (Html Msg)
viewActivity data currentTrial optionsOrder =
    [ viewAudioButton data.state.remainingListenings currentTrial.audio.url
    , div
        [ class "spelling1 center-items justify-center", disabled data.feedback ]
        [ if data.state.step == Answering then
            div [] <| View.shuffledOptions data.state data.feedback UserClickedRadioButton currentTrial optionsOrder

          else
            div [] []
        , View.genericSingleChoiceFeedback
            { isVisible = data.feedback
            , userAnswer = data.state.userAnswer
            , target = currentTrial.target
            , feedback_Correct = ( data.infos.feedback_correct, [ View.bold currentTrial.target ] )
            , feedback_Incorrect = ( data.infos.feedback_incorrect, [ View.bold currentTrial.target ] )
            , button = View.navigationButton UserClickedFeedback UserClickedNextTrial data.feedback data.state.userAnswer
            }
        ]
    ]


view : Model a -> Html Msg
view model =
    case model.spelling1 of
        Activity.Loading _ _ ->
            View.loading

        Activity.NotStarted ->
            text "I'm not started yet"

        Activity.Running Activity.Instructions data ->
            div [ class "flex flex-col items-center" ] [ View.instructions data.infos UserClickedStartTraining ]

        Activity.Err reason ->
            text <| "Error: " ++ reason

        Activity.Running Activity.Training ({ current, history } as data) ->
            case current of
                Just x ->
                    div [ class "w-full flex flex-col justify-center items-center" ] <|
                        View.trainingWheelsGeneric
                            (List.length history)
                            data.infos.trainingWheel
                            [ View.bold x.target ]
                            :: viewActivity data x model.optionsOrder

                Nothing ->
                    View.introToMain UserClickedStartMainloop

        Activity.Running Activity.Main ({ current, infos } as data) ->
            case current of
                Just trial ->
                    div [ class "flex flex-col justify-center items-center" ]
                        (viewActivity
                            data
                            trial
                            model.optionsOrder
                        )

                Nothing ->
                    View.end infos.end UserClickedSavedData (Just "context-understanding")


viewAudioButton : Int -> String -> Html Msg
viewAudioButton nTimes url =
    case nTimes of
        3 ->
            View.audioButton UserClickedPlayAudio url "Listen"

        2 ->
            View.audioButton UserClickedPlayAudio url "Listen again?"

        1 ->
            View.audioButton UserClickedPlayAudio url "Listen for the last time?"

        _ ->
            div [] []



-- UPDATE


type Msg
    = GotTrials (RemoteData Http.Error (List Trial))
    | GotRandomizedTrials (List Trial)
    | UserClickedNextTrial
    | NextTrial Time.Posix
    | UserClickedFeedback
    | UserClickedRadioButton String
    | UserClickedStartMainloop
    | UserClickedSavedData
    | UserClickedPlayAudio String
    | UserClickedStartTraining
    | RuntimeShuffledOptionsOrder (List Int)
    | AudioEnded { eventType : String, name : String, timestamp : Int }
    | HistoryWasSaved (Result Http.Error String)


update : Msg -> Model a -> ( Model a, Cmd Msg )
update msg model =
    let
        currentSpellingState =
            Activity.getState model.spelling1 |> Maybe.withDefault initState
    in
    case msg of
        GotTrials (RemoteData.Success trials) ->
            ( model
            , Random.generate GotRandomizedTrials (shuffle trials)
            )

        GotRandomizedTrials trials ->
            ( { model | spelling1 = Activity.trialsLoaded trials initState model.spelling1 }
            , Cmd.none
            )

        GotTrials (RemoteData.Failure error) ->
            ( { model | spelling1 = Activity.Err (Data.buildErrorMessage error) }
            , Cmd.none
            )

        GotTrials _ ->
            ( model, Cmd.none )

        UserClickedFeedback ->
            ( { model
                | spelling1 =
                    model.spelling1
                        |> Activity.toggle
              }
            , Cmd.none
            )

        UserClickedRadioButton newChoice ->
            ( { model
                | spelling1 =
                    Activity.update { currentSpellingState | userAnswer = newChoice } model.spelling1
              }
            , Cmd.none
            )

        UserClickedNextTrial ->
            ( model, Task.perform NextTrial Time.now )

        NextTrial timestamp ->
            let
                newModel =
                    { model | spelling1 = Activity.next timestamp initState model.spelling1 }
            in
            ( newModel
            , Cmd.batch
                [ saveData newModel
                , Random.generate RuntimeShuffledOptionsOrder (Random.List.shuffle model.optionsOrder)
                ]
            )

        RuntimeShuffledOptionsOrder newOrder ->
            ( { model | optionsOrder = newOrder }, Cmd.none )

        UserClickedStartMainloop ->
            ( { model | spelling1 = Activity.startMain model.spelling1 initState }, Cmd.none )

        -- data is now saved after each "trial", so this does nothing and shoud be removed
        UserClickedSavedData ->
            ( model, Cmd.none )

        UserClickedPlayAudio url ->
            ( { model | spelling1 = Activity.update { currentSpellingState | remainingListenings = currentSpellingState.remainingListenings - 1 } model.spelling1 }, Ports.playAudio url )

        UserClickedStartTraining ->
            ( { model | spelling1 = Activity.startTraining model.spelling1 }, Cmd.none )

        AudioEnded { eventType } ->
            case eventType of
                "SoundEnded" ->
                    ( { model | spelling1 = Activity.update { currentSpellingState | step = Answering } model.spelling1 }, Cmd.none )

                _ ->
                    ( model, Cmd.none )

        HistoryWasSaved _ ->
            ( model, Cmd.none )



-- SUBSCRIPTIONS


subscriptions : Model a -> Sub Msg
subscriptions model =
    case model.spelling1 of
        Activity.Running _ { state } ->
            case state.step of
                ListeningFirstTime ->
                    Sub.batch [ Ports.audioEnded AudioEnded ]

                _ ->
                    Sub.none

        _ ->
            Sub.none



-- HTTP


decodeTrials : Decode.Decoder (List Trial)
decodeTrials =
    let
        decoder =
            Decode.succeed Trial
                |> required "UID" Decode.string
                |> required "Word_Text" Decode.string
                |> required "Distractor_1_CCS" Decode.string
                |> required "Distractor_2_CCS" Decode.string
                |> required "Distractor_3_CCS" Decode.string
                |> optional "isTraining" Decode.bool False
                |> required "Word_Audio" Data.decodeAudioFiles
    in
    decodeRecords decoder


getRecords : String -> Cmd Msg
getRecords group =
    Http.get
        { url =
            Url.Builder.absolute [ ".netlify", "functions", "api" ]
                [ Url.Builder.string "base" "input"
                , Url.Builder.string "view" "Presentation"
                , Url.Builder.string "filterByFormula" ("{Classe} = \"" ++ group ++ "\"")
                ]
        , expect = Http.expectJson (RemoteData.fromResult >> GotTrials) decodeTrials
        }


saveData : Model superModel -> Cmd Msg
saveData model =
    let
        history =
            Activity.getHistory model.spelling1
                |> List.filter (\( trial, _, _ ) -> not trial.isTraining)

        userId =
            model.user |> Maybe.withDefault "recd18l2IBRQNI05y"

        payload =
            updateHistoryEncoder userId history
    in
    Http.request
        { method = "PATCH"
        , headers = []
        , url = Data.buildQuery { app = Data.apps.spacing, base = "users", view_ = "Session1_output" }
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
                , ( "fields", historyEncoder history )
                ]
        )
        [ ( userId, history ) ]


historyEncoder : List ( Trial, State, Time.Posix ) -> Encode.Value
historyEncoder history =
    Encode.object
        -- airtable does not support JSON columns, so we save giant JSON strings
        [ ( "Spelling1", Encode.string (Encode.encode 0 (Encode.list historyItemEncoder history)) )
        ]


historyItemEncoder : ( Trial, State, Time.Posix ) -> Encode.Value
historyItemEncoder ( { uid, target }, { userAnswer }, timestamp ) =
    Encode.object
        [ ( "trialUid", Encode.string uid )
        , ( "target", Encode.string target )
        , ( "answer", Encode.string userAnswer )
        , ( "answeredAt", Encode.int (Time.posixToMillis timestamp) )
        ]
