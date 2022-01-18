module Session2.Translation exposing (..)

import Data
import Dict
import ExperimentInfo
import Html.Styled
    exposing
        ( Html
        , div
        , text
        )
import Html.Styled.Attributes exposing (class, disabled)
import Http
import Json.Decode as Decode exposing (Decoder, string)
import Json.Decode.Pipeline exposing (..)
import Json.Encode as Encode
import Logic
import Random
import Random.List
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



-- VIEW


view : { task : Logic.Task Trial State, optionsOrder : List comparable } -> Html Msg
view task =
    case task.task of
        Logic.Running Logic.Training data ->
            case data.current of
                Just trial ->
                    div [ class "flex flex-col items-center" ]
                        [ View.trainingWheelsGeneric (List.length data.history) data.infos.trainingWheel [ trial.target ]
                        , renderTask task trial data data.history data.trainingTrials
                        ]

                Nothing ->
                    View.introToMain UserClickedStartMain

        Logic.Running Logic.Main data ->
            case data.current of
                Just trial ->
                    div [ class "flex flex-col items-center" ]
                        [ renderTask task trial data data.history data.mainTrials
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


renderTask task trial data history allTrials =
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
            let
                newModel =
                    { model | translationTask = Logic.next initState model.translationTask }
            in
            ( newModel
            , Cmd.batch
                [ Random.generate RuntimeShuffledOptionsOrder (Random.List.shuffle model.optionsOrder)
                , saveData newModel
                ]
            )

        UserClickedToggleFeedback ->
            ( { model | translationTask = Logic.toggle model.translationTask }, Cmd.none )

        UserClickedRadioButton newChoice ->
            ( { model | translationTask = Logic.update { uid = "", userAnswer = newChoice } model.translationTask }, Cmd.none )

        UserClickedStartTraining ->
            ( { model | translationTask = Logic.startTraining model.translationTask }, Cmd.none )

        UserClickedStartMain ->
            ( { model | translationTask = Logic.startMain model.translationTask initState }, Cmd.none )

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
            Logic.getHistory model.translationTask
                |> List.filter (\( trial, _ ) -> not trial.isTraining)

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


updateHistoryEncoder : String -> List ( Trial, State ) -> Encode.Value
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


historyEncoder : String -> List ( Trial, State ) -> Encode.Value
historyEncoder userId history =
    Encode.object
        -- airtable does not support JSON columns, so we save giant JSON strings
        [ ( "Meaning2", Encode.string (Encode.encode 0 (Encode.list historyItemEncoder history)) )
        ]


historyItemEncoder : ( Trial, State ) -> Encode.Value
historyItemEncoder ( { uid, target }, { userAnswer } ) =
    Encode.object
        [ ( "trialUid", Encode.string uid )
        , ( "target", Encode.string target )
        , ( "answser", Encode.string userAnswer )
        ]



-- INTERNALS


taskId =
    "recf5HANE632FLKbc"
