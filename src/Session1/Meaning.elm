module Session1.Meaning exposing (..)

import Browser
import Css exposing (visibility)
import Data exposing (decodeRecords)
import Dict
import Experiment.Experiment as E
import ExperimentInfo exposing (Task)
import Html
import Html.Styled
    exposing
        ( Html
        , div
        , fieldset
        , h1
        , h2
        , h3
        , h4
        , h5
        , h6
        , input
        , label
        , p
        , span
        , text
        )
import Html.Styled.Attributes exposing (checked, class, disabled, for, id, type_)
import Http
import Json.Decode as Decode exposing (Decoder, string)
import Json.Decode.Pipeline exposing (..)
import Logic
import Progressbar
import Session3.Synonym as Synonym
import String.Interpolate exposing (interpolate)
import Url exposing (Url)
import Url.Builder
import View


taskId =
    "rec9fDmVOpqDJktmQ"


type Msg
    = UserClickedNextTrial
    | UserClickedToggleFeedback
    | UserClickedRadioButton String
    | UserClickedStartIntro (List Trial)
    | UserClickedStartMain (List Trial) ExperimentInfo.Task
    | SaveDataMsg
    | ServerRespondedWithLastRecords (Result Http.Error (List String))


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
                |> Data.decodeBool "isTraining"
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
            [ text <| String.fromInt (trialn + 1) ++ ". "
            , span [ class "italic" ] [ text word ]
            ]
        ]


view :
    { task : Logic.Task Trial State
    , infos : Maybe ExperimentInfo.Task
    , radioMsg : String -> msg
    , toggleFeedbackMsg : msg
    , nextTrialMsg : msg
    , optionsOrder : List comparable
    , startMainMsg : List Trial -> Task -> msg
    , saveDataMsg : msg
    }
    -> Html msg
view task =
    case task.task of
        Logic.Loading ->
            div [] [ text "Loading... " ]

        Logic.Err reason ->
            div [] [ text reason ]

        Logic.Intr data ->
            case data.current of
                Just trial ->
                    View.viewTraining data.infos.instructions
                        [ p [ class "col-start-2 col-span-4" ] [ viewQuestion trial.writtenWord (List.length data.history) ]
                        , div [ class "p-2 col-start-2 col-span-4" ]
                            [ div
                                [ class "pt-6 max-w-xl ", disabled data.feedback ]
                              <|
                                View.shuffledOptions
                                    data.state
                                    data.feedback
                                    task.radioMsg
                                    trial
                                    task.optionsOrder
                            ]
                        , div [ class "col-start-2 col-span-4" ] <|
                            if data.feedback then
                                [ if data.state.userAnswer /= trial.target then
                                    View.fromMarkdown trial.feedbackCorrect

                                  else
                                    View.fromMarkdown trial.feedbackIncorrect
                                , View.button
                                    { message = task.nextTrialMsg
                                    , isDisabled = False
                                    , txt = "Next Training Item"
                                    }
                                ]

                            else
                                [ View.button
                                    { message = task.toggleFeedbackMsg
                                    , isDisabled = False
                                    , txt = "Check my answer"
                                    }
                                ]
                        ]

                Nothing ->
                    div [ class "flex flex-col" ]
                        [ h2 [] [ text "The Training is Over 👍️ " ]
                        , text "Now you understand the activity, let's try our target words."
                        , View.button
                            { message = task.startMainMsg data.mainTrials data.infos
                            , txt = "Start"
                            , isDisabled = False
                            }
                        ]

        Logic.Main data ->
            case data.current of
                Just trial ->
                    div []
                        [ p [ class "col-start-2 col-span-4" ] [ viewQuestion trial.writtenWord (List.length data.history) ]
                        , div [ class "p-2 col-start-2 col-span-4" ]
                            [ div
                                [ class "pt-6 max-w-xl ", disabled data.feedback ]
                              <|
                                View.shuffledOptions
                                    data.state
                                    data.feedback
                                    task.radioMsg
                                    trial
                                    task.optionsOrder
                            ]
                        , div [ class "col-start-2 col-span-4" ] <|
                            if data.feedback then
                                [ if data.state.userAnswer /= trial.target then
                                    View.fromMarkdown trial.feedbackCorrect

                                  else
                                    View.fromMarkdown trial.feedbackIncorrect
                                , View.button
                                    { message = task.nextTrialMsg
                                    , isDisabled = False
                                    , txt = "Next Item"
                                    }
                                ]

                            else
                                [ View.button
                                    { message = task.toggleFeedbackMsg
                                    , isDisabled = False
                                    , txt = "Check my answer"
                                    }
                                ]
                        ]

                Nothing ->
                    View.end data.infos.end task.saveDataMsg

        Logic.NotStarted ->
            div [] [ text "I did not start yet." ]


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
