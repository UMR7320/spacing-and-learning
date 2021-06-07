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
import Progressbar
import Random
import Random.List
import View



--getTrialsFromServer : Decodera -> b


getTrialsFromServer : (Result Http.Error (List Trial) -> msg) -> Cmd msg
getTrialsFromServer msgHandler =
    Data.getTrialsFromServer_ "input" "Translation" msgHandler decodeTrials



-- Translation


type Msg
    = UserClickedNextTrial
    | UserClickedToggleFeedback
    | UserClickedSaveData
    | UserClickedRadioButton String
    | UserClickedStartTraining
    | UserClickedStartMain
    | ServerRespondedWithLastRecords (Result Http.Error (List ()))
    | RuntimeShuffledOptionsOrder (List Int)


update msg model =
    case msg of
        UserClickedNextTrial ->
            ( { model | translationTask = Logic.next initState model.translationTask }
            , Cmd.batch
                [ Random.generate RuntimeShuffledOptionsOrder (Random.List.shuffle model.optionsOrder)
                ]
            )

        ServerRespondedWithLastRecords (Ok _) ->
            ( model, Cmd.none )

        ServerRespondedWithLastRecords (Err _) ->
            ( model, Cmd.none )

        UserClickedToggleFeedback ->
            ( { model | translationTask = Logic.toggle model.translationTask }, Cmd.none )

        UserClickedRadioButton newChoice ->
            ( { model | translationTask = Logic.update { uid = "", userAnswer = newChoice } model.translationTask }, Cmd.none )

        UserClickedStartTraining ->
            ( { model | translationTask = Logic.startTraining model.translationTask }, Cmd.none )

        UserClickedStartMain ->
            ( { model | translationTask = Logic.startMain model.translationTask initState }, Cmd.none )

        UserClickedSaveData ->
            let
                responseHandler =
                    ServerRespondedWithLastRecords
            in
            ( model, Logic.saveData responseHandler model.user taskId model.translationTask )

        RuntimeShuffledOptionsOrder ls ->
            ( { model | optionsOrder = ls }, Cmd.none )


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


type alias State =
    { uid : String
    , userAnswer : String
    }


initState : State
initState =
    { uid = ""
    , userAnswer = ""
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


type alias SummarizedTrial =
    { trialuid : String, userUid : String, attempt : String }


encodeHistory : SummarizedTrial -> Encode.Value
encodeHistory trial =
    Encode.object [ ( "userUid", Encode.string trial.userUid ), ( "trialUid", Encode.string trial.trialuid ), ( "attempt", Encode.string trial.attempt ) ]


payload history =
    Encode.list encodeHistory history


renderTask task trial data history allTrials =
    div [ class "text-2xl w-1/2 p-2" ]
        [ Progressbar.progressBar history allTrials
        , View.fromMarkdown trial.question
        , div
            [ class "w-full max-w-1/3 pt-8", disabled data.feedback ]
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


view :
    { task : Logic.Task Trial State
    , optionsOrder : List comparable
    }
    -> Html Msg
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
            div [] [ text "Loading" ]

        Logic.NotStarted ->
            div [] [ text "The experiment is not started yet" ]

        Logic.Err reason ->
            div [] [ text reason ]

        Logic.Running Logic.Instructions data ->
            div [] [ View.instructions data.infos.instructions UserClickedStartTraining ]


taskId =
    "recf5HANE632FLKbc"


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
