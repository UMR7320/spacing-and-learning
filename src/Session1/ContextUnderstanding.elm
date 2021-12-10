module Session1.ContextUnderstanding exposing (..)

import Data
import Dict
import ExperimentInfo
import Html.Styled exposing (Html, div, p, span, text)
import Html.Styled.Attributes exposing (class)
import Http exposing (Error)
import Json.Decode as Decode exposing (Decoder, string)
import Json.Decode.Pipeline exposing (..)
import Json.Encode as Encode
import Logic
import Random
import Random.List
import View



-- MODEL


type alias Trial =
    { uid : String
    , text : String
    , target : String
    , distractor1 : String
    , distractor2 : String
    , distractor3 : String
    , definition : String
    , isTraining : Bool
    }


type alias State =
    { uid : String
    , userAnswer : String
    }


type alias CU =
    Logic.Task Trial State


initState : State
initState =
    State "DefaultUid" ""


defaultTrial : Trial
defaultTrial =
    Trial "defaultuid" "defaulttarger" "defaultText" "distractor1" "distractor2" "distractor3" "definition" False


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
        Logic.NotStarted ->
            div [] [ text "experiment did not start yet" ]

        Logic.Err reason ->
            div [] [ text reason ]

        Logic.Running Logic.Training data ->
            case data.current of
                Just trial ->
                    let
                        ( pre, post ) =
                            case String.split "/" trial.text of
                                x :: y :: _ ->
                                    ( x, y )

                                [ x ] ->
                                    ( x, "defaultPost" )

                                [] ->
                                    ( "defautpre", "defaultpOst" )
                    in
                    div [ class "flex flex-col items-center" ]
                        [ paragraphWithInput pre data.state.userAnswer post
                        , div [ class "w-full" ] <| View.shuffledOptions data.state data.feedback UserClickedRadioButton trial task.optionsOrder
                        , div [ class "col-start-2 col-span-4" ] <|
                            [ View.genericSingleChoiceFeedback
                                { isVisible = data.feedback
                                , userAnswer = data.state.userAnswer
                                , target = trial.target
                                , feedback_Correct = ( data.infos.feedback_correct, [ View.bold trial.target, View.bold trial.definition ] )
                                , feedback_Incorrect = ( data.infos.feedback_incorrect, [ View.bold trial.target, View.bold trial.definition ] )
                                , button = View.navigationButton UserClickedToggleFeedback UserClickedNextTrial data.feedback data.state.userAnswer
                                }
                            ]
                        ]

                Nothing ->
                    View.introToMain (UserClickedStartMain data.mainTrials data.infos)

        Logic.Running Logic.Main data ->
            case data.current of
                Just trial ->
                    let
                        ( pre, post ) =
                            case String.split "/" trial.text of
                                x :: y :: _ ->
                                    ( x, y )

                                [ x ] ->
                                    ( x, "defaultPost" )

                                [] ->
                                    ( "defautpre", "defaultpOst" )
                    in
                    div [ class "flex flex-col w-full items-center justify-center " ]
                        [ paragraphWithInput pre data.state.userAnswer post
                        , div [ class "w-full" ] <| View.shuffledOptions data.state data.feedback UserClickedRadioButton trial task.optionsOrder
                        , View.genericSingleChoiceFeedback
                            { isVisible = data.feedback
                            , userAnswer = data.state.userAnswer
                            , target = trial.target
                            , feedback_Correct = ( data.infos.feedback_correct, [ View.bold trial.target, View.bold trial.definition ] )
                            , feedback_Incorrect = ( data.infos.feedback_incorrect, [ View.bold trial.target, View.bold trial.definition ] )
                            , button = View.navigationButton UserClickedToggleFeedback UserClickedNextTrial data.feedback data.state.userAnswer
                            }
                        ]

                Nothing ->
                    View.end data.infos.end UserClickedSaveData "../post-tests/cw?session=S1"

        Logic.Loading ->
            View.loading

        Logic.Running Logic.Instructions data ->
            div [] [ View.instructions data.infos UserClickedStartTraining ]


paragraphWithInput pre userAnswer post =
    p [ class "bg-gray-200 mb-8 rounded-lg p-4" ]
        [ text pre
        , span [ class "border-4 h-2 pl-12 pr-12 font-bold" ]
            [ text <|
                if userAnswer == "I don't know" then
                    "???"

                else
                    userAnswer
            ]
        , text post
        ]



-- UPDATE


type Msg
    = UserClickedNextTrial
    | UserClickedToggleFeedback
    | UserClickedRadioButton String
    | UserClickedStartMain (List Trial) ExperimentInfo.Task
    | UserClickedSaveData
    | UserClickedStartTraining
    | RuntimeShuffledOptionsOrder (List Int)
    | HistoryWasSaved (Result Http.Error String)


update msg model =
    case msg of
        UserClickedNextTrial ->
            let
                newModel =
                    { model | cu1 = Logic.next initState model.cu1 }
            in
            ( newModel
            , Cmd.batch
                [ Random.generate RuntimeShuffledOptionsOrder (Random.List.shuffle model.optionsOrder)
                , saveData newModel
                ]
            )

        UserClickedToggleFeedback ->
            ( { model | cu1 = Logic.toggle model.cu1 }, Cmd.none )

        UserClickedRadioButton newChoice ->
            ( { model | cu1 = Logic.update { uid = "", userAnswer = newChoice } model.cu1 }, Cmd.none )

        UserClickedStartMain _ _ ->
            ( { model | cu1 = Logic.startMain model.cu1 initState }, Cmd.none )

        -- data is now saved after each "trial", so this does nothing and shoud be removed
        UserClickedSaveData ->
            ( model, Cmd.none )

        UserClickedStartTraining ->
            ( { model | cu1 = Logic.startTraining model.cu1 }, Cmd.none )

        RuntimeShuffledOptionsOrder newOrder ->
            ( { model | optionsOrder = newOrder }, Cmd.none )

        HistoryWasSaved _ ->
            ( model, Cmd.none )



-- HTTP


getTrialsFromServer : (Result Error (List Trial) -> msg) -> Cmd msg
getTrialsFromServer msgHandler =
    Data.getTrialsFromServer_ "input" "ContextUnderstandingLvl1" msgHandler decodeTranslationInput


decodeTranslationInput : Decoder (List Trial)
decodeTranslationInput =
    let
        decoder =
            Decode.succeed Trial
                |> required "UID" string
                |> required "Text_To_Complete" string
                |> required "Target_CU1" string
                |> required "Distractor_1_CU_Lvl1" string
                |> required "Distractor_2_CU_Lvl1" string
                |> required "Distractor_3_CU_Lvl1" string
                |> required "Definition" string
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
            Logic.getHistory model.cu1
                |> List.filter (\( trial, _ ) -> not trial.isTraining)

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
        [ ( "CU1", Encode.string (Encode.encode 0 (Encode.list historyItemEncoder history)) )
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
    "recsN8oyy3LIC8URx"
