module Session2.Meaning2 exposing (..)

import Activity exposing (Activity)
import ActivityInfo exposing (ActivityInfo, Session(..))
import Data
import Html.Styled exposing (Html, div, text)
import Html.Styled.Attributes exposing (class, disabled)
import Http
import Json.Decode as Decode exposing (Decoder, string)
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
    Activity Trial State


type alias Model superModel =
    { superModel
        | meaning2 : Meaning2
        , user : Maybe String
        , optionsOrder : List Int
    }


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


infoLoaded : List ActivityInfo -> Meaning2 -> Meaning2
infoLoaded infos =
    Activity.infoLoaded
        Session2
        "Meaning 2"
        infos
        initState


init : String -> Model a -> ( Model a, Cmd Msg )
init group model =
    ( { model | meaning2 = Activity.loading model.meaning2 }
    , Cmd.batch
        [ getRecords group
        , Ports.enableAlertOnExit ()
        ]
    )



-- VIEW


view : Model a -> Html Msg
view model =
    case model.meaning2 of
        Activity.Running Activity.Training data ->
            case data.current of
                Just trial ->
                    div [ class "flex flex-col items-center" ]
                        [ renderActivity model trial data ]

                Nothing ->
                    View.introToMain UserClickedStartMain

        Activity.Running Activity.Main data ->
            case data.current of
                Just trial ->
                    div [ class "flex flex-col items-center" ]
                        [ renderActivity model trial data
                        ]

                Nothing ->
                    View.end data.infos.end UserClickedSaveData (Just "spelling")

        Activity.Loading _ _ ->
            View.loading

        Activity.NotStarted ->
            div [] [ text "The experiment is not started yet" ]

        Activity.Err reason ->
            div [] [ text reason ]

        Activity.Running Activity.Instructions data ->
            div [] [ View.instructions data.infos UserClickedStartTraining ]


renderActivity : Model a -> Trial -> Activity.Data Trial State -> Html Msg
renderActivity model trial data =
    div [ class "w-full" ]
        [ View.fromMarkdown trial.question
        , div
            [ class "w-full pt-8", disabled data.feedback ]
            [ View.shuffledOptions
                data.state
                data.feedback
                UserClickedRadioButton
                trial
                model.optionsOrder
            ]
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
    = GotTrials (RemoteData Http.Error (List Trial))
    | GotRandomizedTrials (List Trial)
    | UserClickedNextTrial
    | NextTrial Time.Posix
    | UserClickedToggleFeedback
    | UserClickedSaveData
    | UserClickedRadioButton String
    | UserClickedStartTraining
    | UserClickedStartMain
    | RuntimeShuffledOptionsOrder (List Int)
    | HistoryWasSaved (Result Http.Error String)


update : Msg -> Model a -> ( Model a, Cmd Msg )
update msg model =
    case msg of
        GotTrials (RemoteData.Success trials) ->
            ( model
            , Random.generate GotRandomizedTrials (shuffle trials)
            )

        GotRandomizedTrials trials ->
            ( { model | meaning2 = Activity.trialsLoaded trials initState model.meaning2 }
            , Cmd.none
            )

        GotTrials (RemoteData.Failure error) ->
            ( { model
                | meaning2 = Activity.Err (Data.buildErrorMessage error)
              }
            , Cmd.none
            )

        GotTrials _ ->
            ( model, Cmd.none )

        UserClickedNextTrial ->
            ( model, Task.perform NextTrial Time.now )

        NextTrial timestamp ->
            let
                newModel =
                    { model | meaning2 = Activity.next timestamp initState model.meaning2 }
            in
            ( newModel
            , Cmd.batch
                [ Random.generate RuntimeShuffledOptionsOrder (Random.List.shuffle model.optionsOrder)
                , saveData newModel
                ]
            )

        UserClickedToggleFeedback ->
            ( { model | meaning2 = Activity.toggle model.meaning2 }, Cmd.none )

        UserClickedRadioButton newChoice ->
            ( { model | meaning2 = Activity.update { uid = "", userAnswer = newChoice } model.meaning2 }, Cmd.none )

        UserClickedStartTraining ->
            ( { model | meaning2 = Activity.startTraining model.meaning2 }, Cmd.none )

        UserClickedStartMain ->
            ( { model | meaning2 = Activity.startMain model.meaning2 initState }, Cmd.none )

        -- data is now saved after each "trial", so this does nothing and shoud be removed
        UserClickedSaveData ->
            ( model, Cmd.none )

        RuntimeShuffledOptionsOrder ls ->
            ( { model | optionsOrder = ls }, Cmd.none )

        HistoryWasSaved _ ->
            ( model, Cmd.none )



-- HTTP


getRecords : String -> Cmd Msg
getRecords group =
    Http.get
        { url =
            Url.Builder.absolute [ ".netlify", "functions", "api" ]
                [ Url.Builder.string "base" "input"
                , Url.Builder.string "view" "Meaning"
                , Url.Builder.string "filterByFormula" ("{Classe} = \"" ++ group ++ "\"")
                ]
        , expect = Http.expectJson (RemoteData.fromResult >> GotTrials) decodeTrials
        }


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


saveData : Model a -> Cmd Msg
saveData model =
    let
        history =
            Activity.getHistory model.meaning2
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
                , ( "fields", historyEncoder history )
                ]
        )
        [ ( userId, history ) ]


historyEncoder : List ( Trial, State, Time.Posix ) -> Encode.Value
historyEncoder history =
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
