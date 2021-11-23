module Session1.ContextUnderstanding exposing (..)

import Data
import Dict
import ExperimentInfo
import Html.Styled exposing (Html, div, p, span, text)
import Html.Styled.Attributes exposing (class)
import Http exposing (Error)
import Json.Decode as Decode exposing (Decoder, string)
import Json.Decode.Pipeline exposing (..)
import Logic
import Progressbar
import Random
import Random.List
import View



--view : { a | task : Logic.Task t s, optionsOrder : Maybe (List comparable), nextTrialMsg : f, userClickedAudio : String -> f, radioMsg : String -> f, toggleFeedbackMsg : f, startMainMsg : List t -> f, inputChangedMsg : String -> f } -> Html msg


type alias CU =
    Logic.Task Trial State


taskId =
    "recsN8oyy3LIC8URx"


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


view :
    { task : Logic.Task Trial State
    , optionsOrder : List comparable
    }
    -> Html Msg
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


type Msg
    = UserClickedNextTrial
    | UserClickedToggleFeedback
    | UserClickedRadioButton String
    | UserClickedStartMain (List Trial) ExperimentInfo.Task
    | UserClickedSaveData
    | ServerRespondedWithLastRecords (Result Http.Error (List ()))
    | UserClickedStartTraining
    | RuntimeShuffledOptionsOrder (List Int)


getTrialsFromServer : (Result Error (List Trial) -> msg) -> Cmd msg
getTrialsFromServer msgHandler =
    Data.getTrialsFromServer_ "input" "ContextUnderstandingLvl1" msgHandler decodeTranslationInput


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


update msg model =
    case msg of
        UserClickedNextTrial ->
            ( { model | cu1 = Logic.next initState model.cu1 }, Random.generate RuntimeShuffledOptionsOrder (Random.List.shuffle model.optionsOrder) )

        UserClickedToggleFeedback ->
            ( { model | cu1 = Logic.toggle model.cu1 }, Cmd.none )

        UserClickedRadioButton newChoice ->
            ( { model | cu1 = Logic.update { uid = "", userAnswer = newChoice } model.cu1 }, Cmd.none )

        UserClickedStartMain _ _ ->
            ( { model | cu1 = Logic.startMain model.cu1 initState }, Cmd.none )

        UserClickedSaveData ->
            let
                responseHandler =
                    ServerRespondedWithLastRecords
            in
            ( model, Logic.saveData responseHandler model.user taskId model.cu1 )

        ServerRespondedWithLastRecords (Result.Ok _) ->
            ( model, Cmd.none )

        ServerRespondedWithLastRecords (Err _) ->
            ( model, Cmd.none )

        UserClickedStartTraining ->
            ( { model | cu1 = Logic.startTraining model.cu1 }, Cmd.none )

        RuntimeShuffledOptionsOrder newOrder ->
            ( { model | optionsOrder = newOrder }, Cmd.none )


decodeTranslationInput : Decoder (List Trial)
decodeTranslationInput =
    let
        decoder =
            Decode.succeed Trial
                |> required "UID" string
                |> required "Text_To_Complete" string
                |> required "Word_Text" string
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


initState : State
initState =
    State "DefaultUid" ""


defaultTrial : Trial
defaultTrial =
    Trial "defaultuid" "defaulttarger" "defaultText" "distractor1" "distractor2" "distractor3" "definition" False


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
