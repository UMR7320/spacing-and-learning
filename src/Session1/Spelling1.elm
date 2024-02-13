module Session1.Spelling1 exposing (..)

import Data exposing (decodeRecords)
import ActivityInfo exposing (ActivityInfo, Session(..))
import Html.Styled exposing (Html, div, h2, p, pre, span, text)
import Html.Styled.Attributes exposing (class, disabled)
import Http
import Json.Decode as Decode
import Json.Decode.Pipeline exposing (..)
import Json.Encode as Encode
import Activity exposing (Activity)
import Ports
import Random
import Random.List
import Task
import Time
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


initState : State
initState =
    State "DefaultTrialUID" "" 3 ListeningFirstTime


start info trials =
    Activity.startIntro
        (ActivityInfo.activityInfo info Session1 "Spelling 1")
        (List.filter (\datum -> datum.isTraining) trials)
        (List.filter (\datum -> not datum.isTraining) trials)
        initState


infoLoaded : List ActivityInfo -> Spelling1 -> Spelling1
infoLoaded infos =
    Activity.infoLoaded
        Session1
        "Spelling 1"
        infos
        initState




-- VIEW


viewInstructions txt =
    div [ class "flex flex-col" ]
        [ h2 [ class "font-bold" ] [ text "Instructions" ]
        , p [ class "pt-8 pb-8 font-medium" ]
            [ pre [] [ text txt ]
            ]
        , div [ class "text-green-500 font-bold pb-2" ] [ span [] [ text "Practice here!" ] ]
        ]


trainingBox =
    div [ class "w-full h-full border-4 border-green-500 border-rounded-lg border-dashed flex items-center justify-center flex-col" ]


viewActivity data currentTrial optionsOrder =
    [ viewAudioButton data.state.remainingListenings currentTrial.audio.url
    , div
        [ class "pt-6 center-items justify-center max-w-xl w-full mt-6 ", disabled data.feedback ]
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


view : Activity Trial State -> List Int -> Html Msg
view exp optionsOrder =
    case exp of
        Activity.Loading _ _ ->
            View.loading

        Activity.NotStarted ->
            text "I'm not started yet"

        Activity.Running Activity.Instructions data ->
            div [ class "flex flex-col items-center" ] [ View.instructions data.infos UserClickedStartTraining ]

        Activity.Err reason ->
            text <| "Error: " ++ reason

        Activity.Running Activity.Training ({ trainingTrials, mainTrials, current, state, feedback, history, infos } as data) ->
            case current of
                Just x ->
                    div [ class "w-full flex flex-col justify-center items-center" ] <|
                        View.trainingWheelsGeneric
                            (List.length history)
                            data.infos.trainingWheel
                            [ View.bold x.target ]
                            :: viewActivity data x optionsOrder

                Nothing ->
                    View.introToMain UserClickedStartMainloop

        Activity.Running Activity.Main ({ mainTrials, current, state, feedback, history, infos } as data) ->
            case current of
                Just trial ->
                    div [ class "flex flex-col justify-center items-center" ]
                        (viewActivity
                            data
                            trial
                            optionsOrder
                        )

                Nothing ->
                    View.end infos.end UserClickedSavedData (Just "context-understanding")


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
    = UserClickedNextTrial
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


update msg model =
    let
        currentSpellingState =
            Activity.getState model.spelling1 |> Maybe.withDefault initState
    in
    case msg of
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
        stringToBoolDecoder : String -> Decode.Decoder Bool
        stringToBoolDecoder str =
            case str of
                "true" ->
                    Decode.succeed True

                _ ->
                    Decode.succeed False

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
        , resolver = Http.stringResolver <| Data.handleJsonResponse <| decodeTrials
        , timeout = Just 5000
        }


getTrialsFromServer : (Result Http.Error (List Trial) -> msg) -> Cmd msg
getTrialsFromServer callbackMsg =
    Data.getTrialsFromServer_ "input" "Meaning" callbackMsg decodeTrials


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
                , ( "fields", historyEncoder userId history )
                ]
        )
        [ ( userId, history ) ]


historyEncoder : String -> List ( Trial, State, Time.Posix ) -> Encode.Value
historyEncoder userId history =
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
