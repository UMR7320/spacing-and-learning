module Session2.Meaning2 exposing (..)

import Data
import ExperimentInfo exposing (Session(..))
import Html.Styled exposing (Html, div, text)
import Html.Styled.Attributes exposing (class, disabled)
import Http
import Json.Decode as Decode exposing (Decoder, string)
import Json.Decode.Pipeline exposing (..)
import Json.Encode as Encode
import Logic
import Random
import Random.List
import Task
import Time
import View



-- MODEL


type alias SummarizedTrial =
    { trialuid : String, userUid : String, attempt : String }


type alias Trial =
    { uid : String
    , question : String
    , target : String
    , translation2 : String
    , distractor1 : String
    , distractor2 : String
    , distractor3 : String
    , word : String
    , isTraining : Bool
    }


type alias State =
    { uid : String
    , userAnswer : String
    }


type alias Meaning2 =
    Logic.Activity Trial State


defaultTrial : Trial
defaultTrial =
    { uid = ""
    , question = "String"
    , target = "String"
    , translation2 = "String"
    , distractor1 = ""
    , distractor2 = ""
    , distractor3 = ""
    , word = "String"
    , isTraining = False
    }


initState : State
initState =
    { uid = ""
    , userAnswer = ""
    }


start : List ExperimentInfo.Activity -> List Trial -> Logic.Activity Trial State
start info trials =
    Logic.startIntro
        (ExperimentInfo.activityInfo info Session2 "Meaning 2")
        (List.filter (\datum -> datum.isTraining) trials)
        (List.filter (\datum -> not datum.isTraining) trials)
        initState



-- VIEW


view : { task : Logic.Activity Trial State, optionsOrder : List comparable } -> Html Msg
view task =
    case task.task of
        Logic.Running Logic.Training data ->
            case data.current of
                Just trial ->
                    div [ class "flex flex-col items-center" ]
                        [ View.trainingWheelsGeneric (List.length data.history) data.infos.trainingWheel [ trial.target ]
                        , renderActivity task trial data data.history data.trainingTrials
                        ]

                Nothing ->
                    View.introToMain UserClickedStartMain

        Logic.Running Logic.Main data ->
            case data.current of
                Just trial ->
                    div [ class "flex flex-col items-center" ]
                        [ renderActivity task trial data data.history data.mainTrials
                        ]

                Nothing ->
                    View.end data.infos.end UserClickedSaveData "spelling"

        Logic.Loading ->
            View.loading

        Logic.NotStarted ->
            div [] [ text "The experiment is not started yet" ]

        Logic.Err reason ->
            div [] [ text reason ]

        Logic.Running Logic.Instructions data ->
            div [] [ View.instructions data.infos UserClickedStartTraining ]


renderActivity task trial data history allTrials =
    div [ class "w-full" ]
        [ View.fromMarkdown trial.question
        , div
            [ class "w-full pt-8", disabled data.feedback ]
          <|
            View.shuffledOptions
                data.state
                data.feedback
                UserClickedRadioButton
                trial
                task.optionsOrder
        , View.genericSingleChoiceFeedback
            { isVisible = data.feedback
            , userAnswer = data.state.userAnswer
            , target = trial.target
            , feedback_Correct = ( data.infos.feedback_correct, [ trial.target ] )
            , feedback_Incorrect = ( data.infos.feedback_incorrect, [ trial.target ] )
            , button = View.navigationButton UserClickedToggleFeedback UserClickedNextTrial data.feedback data.state.userAnswer
            }
        ]



-- UPDATE


type Msg
    = UserClickedNextTrial
    | NextTrial Time.Posix
    | UserClickedToggleFeedback
    | UserClickedSaveData
    | UserClickedRadioButton String
    | UserClickedStartTraining
    | UserClickedStartMain
    | RuntimeShuffledOptionsOrder (List Int)
    | HistoryWasSaved (Result Http.Error String)


update msg model =
    case msg of
        UserClickedNextTrial ->
            ( model, Task.perform NextTrial Time.now )

        NextTrial timestamp ->
            let
                newModel =
                    { model | meaning2 = Logic.next timestamp initState model.meaning2 }
            in
            ( newModel
            , Cmd.batch
                [ Random.generate RuntimeShuffledOptionsOrder (Random.List.shuffle model.optionsOrder)
                , saveData newModel
                ]
            )

        UserClickedToggleFeedback ->
            ( { model | meaning2 = Logic.toggle model.meaning2 }, Cmd.none )

        UserClickedRadioButton newChoice ->
            ( { model | meaning2 = Logic.update { uid = "", userAnswer = newChoice } model.meaning2 }, Cmd.none )

        UserClickedStartTraining ->
            ( { model | meaning2 = Logic.startTraining model.meaning2 }, Cmd.none )

        UserClickedStartMain ->
            ( { model | meaning2 = Logic.startMain model.meaning2 initState }, Cmd.none )

        -- data is now saved after each "trial", so this does nothing and shoud be removed
        UserClickedSaveData ->
            ( model, Cmd.none )

        RuntimeShuffledOptionsOrder ls ->
            ( { model | optionsOrder = ls }, Cmd.none )

        HistoryWasSaved _ ->
            ( model, Cmd.none )



-- HTTP


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
getTrialsFromServer msgHandler =
    Data.getTrialsFromServer_ "input" "Translation" msgHandler decodeTrials


decodeTrials : Decoder (List Trial)
decodeTrials =
    let
        decoder =
            Decode.succeed Trial
                |> required "UID" string
                |> required "Question_Translation" string
                |> required "Translation_1" string
                |> optional "Translation_2" string "MISSING_TRANS_2"
                |> optional "Distractor_1_Translation" string "Missing distractor"
                |> optional "Distractor_2_Translation" string "Missing distractor"
                |> optional "Distractor_3_Translation" string "missing distractor"
                |> optional "Word_Text" string "MISSING"
                |> optional "isTraining" Decode.bool False
    in
    Data.decodeRecords decoder


saveData model =
    let
        history =
            Logic.getHistory model.meaning2
                |> List.filter (\( trial, _, _ ) -> not trial.isTraining)

        userId =
            model.user |> Maybe.withDefault "recd18l2IBRQNI05y"

        payload =
            updateHistoryEncoder userId history
    in
    Http.request
        { method = "PATCH"
        , headers = []
        , url = Data.buildQuery { app = Data.apps.spacing, base = "users", view_ = "Session2_output" }
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
        [ ( "Meaning2", Encode.string (Encode.encode 0 (Encode.list historyItemEncoder history)) )
        ]


historyItemEncoder : ( Trial, State, Time.Posix ) -> Encode.Value
historyItemEncoder ( { uid, target }, { userAnswer }, timestamp ) =
    Encode.object
        [ ( "trialUid", Encode.string uid )
        , ( "target", Encode.string target )
        , ( "answer", Encode.string userAnswer )
        , ( "answeredAt", Encode.int (Time.posixToMillis timestamp) )
        ]
