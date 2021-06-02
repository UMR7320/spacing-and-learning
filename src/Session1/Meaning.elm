module Session1.Meaning exposing (..)

import Data exposing (decodeRecords)
import Dict
import ExperimentInfo
import Html.Styled
    exposing
        ( Html
        , div
        , h1
        , h3
        , p
        , span
        , text
        )
import Html.Styled.Attributes exposing (class, disabled)
import Http
import Json.Decode as Decode exposing (Decoder, string)
import Json.Decode.Pipeline exposing (..)
import Logic
import Progressbar
import Random
import Random.List
import Session2.Translation exposing (Msg(..))
import String.Interpolate exposing (interpolate)
import View
import Json.Decode exposing (bool)


type alias Meaning =
    Logic.Task Trial State


taskId =
    "rec9fDmVOpqDJktmQ"


type Msg
    = UserClickedNextTrial
    | UserClickedToggleFeedback
    | UserClickedRadioButton String
    | UserClickedStartMain
    | SaveDataMsg
    | ServerRespondedWithLastRecords (Result Http.Error (List ()))
    | UserClickedStartTraining
    | RuntimeShuffledOptionsOrder (List Int)


decodeMeaningInput : Decoder (List Trial)
decodeMeaningInput =
    let
        decoder =
            Decode.succeed Trial
                |> required "UID" string
                |> required "Word_Text" string
                |> required "Definition" string
                |> optional "Distractor_1_Meaning" string "MISSING"
                |> optional "Distractor_2_Meaning" string "MISSING"
                |> optional "Distractor_3_Meaning" string "MISSING"
                |> required "Feedback_Incorrect_Meaning" string
                |> required "Feedback_Correct_Meaning" string
                |> optional "isTraining" bool False
    in
    decodeRecords decoder


getTrialsFromServer : (Result Http.Error (List Trial) -> msg) -> Cmd msg
getTrialsFromServer callbackMsg =
    Data.getTrialsFromServer_ "input" "Meaning" callbackMsg decodeMeaningInput


initState : State
initState =
    State "DefaultTrialUID" ""


defaultTrial : Trial
defaultTrial =
    { uid = "MISSING"
    , writtenWord = "MISSING"
    , target = "MISSING"
    , distractor1 = "MISSING"
    , distractor2 = "MISSING"
    , distractor3 = "MISSING"
    , feedbackCorrect = "MISSING"
    , feedbackIncorrect = "MISSING"
    , isTraining = False
    }


type alias Trial =
    { uid : String
    , writtenWord : String
    , target : String
    , distractor1 : String
    , distractor2 : String
    , distractor3 : String
    , feedbackCorrect : String
    , feedbackIncorrect : String
    , isTraining : Bool
    }


type alias State =
    { uid : String
    , userAnswer : String
    }


viewQuestion word trialn =
    h3 []
        [ p []
            [ span [ class "italic" ] [ text word ]
            ]
        ]


view :
    { task : Logic.Task Trial State
    , optionsOrder : List comparable
    }
    -> Html Msg
view task =
    case task.task of
        Logic.Loading ->
            div [] [ text "Loading... " ]

        Logic.Err reason ->
            div [] [ text reason ]

        Logic.Running Logic.Training data ->
            case data.current of
                Just trial ->
                    div [ class "flex flex-col items-center" ]
                        [ p [] [ View.trainingWheelsGeneric (List.length data.history) data.infos.trainingWheel [ View.bold trial.writtenWord, View.bold trial.target ] ]
                        , p [] [ viewQuestion trial.writtenWord (List.length data.history) ]
                        , div
                            [ class "pt-6 max-w-xl ", disabled data.feedback ]
                          <|
                            View.shuffledOptions
                                data.state
                                data.feedback
                                UserClickedRadioButton
                                trial
                                task.optionsOrder
                        , div []
                            [ View.genericSingleChoiceFeedback
                                { isVisible = data.feedback
                                , userAnswer = data.state.userAnswer
                                , target = trial.target
                                , feedback_Correct = ( trial.feedbackIncorrect, [] )
                                , feedback_Incorrect = ( trial.feedbackCorrect, [] )
                                , button = View.navigationButton UserClickedToggleFeedback UserClickedNextTrial data.feedback
                                }
                            ]
                        ]

                Nothing ->
                    View.introToMain UserClickedStartMain

        Logic.Running Logic.Main data ->
            case data.current of
                Just trial ->
                    div [ class "container flex flex-col items-center justify-center" ]
                        [ Progressbar.progressBar data.history data.mainTrials
                        , View.tooltip (interpolate data.infos.instructions_short [ trial.writtenWord ])
                        , div [ class "mr-8 w-full max-w-xl" ]
                            [ viewQuestion trial.writtenWord (List.length data.history)
                            , div
                                [ class "pt-6 center-items justify-center max-w-xl w-full mt-6 ", disabled data.feedback ]
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
                                , feedback_Correct = ( trial.feedbackIncorrect, [] )
                                , feedback_Incorrect = ( trial.feedbackCorrect, [] )
                                , button = View.navigationButton UserClickedToggleFeedback UserClickedNextTrial data.feedback
                                }
                            ]
                        ]

                Nothing ->
                    View.end data.infos.end SaveDataMsg "spelling"

        Logic.NotStarted ->
            div [] [ text "I did not start yet." ]

        Logic.Running Logic.Instructions data ->
            div []
                [ h1 [] [ text "Instructions" ]
                , p [] [ View.fromMarkdown data.infos.instructions ]
                , View.button
                    { isDisabled = False
                    , message = UserClickedStartTraining
                    , txt = "Start training"
                    }
                ]


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
        , resolver = Http.stringResolver <| Data.handleJsonResponse <| decodeMeaningInput
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


update msg model =
    case msg of
        UserClickedNextTrial ->
            ( { model | meaning = Logic.next initState model.meaning }, Random.generate RuntimeShuffledOptionsOrder (Random.List.shuffle model.optionsOrder) )

        UserClickedToggleFeedback ->
            ( { model | meaning = Logic.toggle model.meaning }, Cmd.none )

        UserClickedRadioButton newChoice ->
            ( { model | meaning = Logic.update { uid = "", userAnswer = newChoice } model.meaning }, Cmd.none )

        UserClickedStartMain ->
            ( { model | meaning = Logic.startMain model.meaning initState }, Cmd.none )

        SaveDataMsg ->
            ( model, Logic.saveData ServerRespondedWithLastRecords model.user taskId model.meaning )

        ServerRespondedWithLastRecords _ ->
            ( model, Cmd.none )

        UserClickedStartTraining ->
            ( { model | meaning = Logic.startTraining model.meaning }, Cmd.none )

        RuntimeShuffledOptionsOrder newOrder ->
            ( { model | optionsOrder = newOrder }, Cmd.none )