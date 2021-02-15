module Experiment.CU1 exposing (..)

import Data
import ExperimentInfo
import Html.Styled as Html exposing (Html, div, fromUnstyled, p, span, text)
import Html.Styled.Attributes exposing (class)
import Html.Styled.Events
import Http exposing (Error)
import Icons
import Json.Decode as Decode exposing (Decoder, string)
import Json.Decode.Pipeline exposing (..)
import PsychTask
import String.Interpolate exposing (interpolate)
import View



--view : { a | task : PsychTask.Task t s, optionsOrder : Maybe (List comparable), nextTrialMsg : f, userClickedAudio : String -> f, radioMsg : String -> f, toggleFeedbackMsg : f, startMainMsg : List t -> f, inputChangedMsg : String -> f } -> Html msg


view :
    { task : PsychTask.Task Trial State
    , infos : Maybe ExperimentInfo.Task
    , radioMsg : String -> msg
    , toggleFeedbackMsg : msg
    , nextTrialMsg : msg
    , optionsOrder : List comparable
    , startMainMsg : List Trial -> msg
    }
    -> Html msg
view task =
    case ( task.task, task.infos ) of
        ( _, Nothing ) ->
            div [] [ text "Error : I can't find the task's infos" ]

        ( PsychTask.NotStartedYet, Just info ) ->
            div [] [ text "experiment did not start yet" ]

        ( PsychTask.Intr data, Just info ) ->
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
                    div []
                        [ View.viewTraining info.instructions
                            [ p [ class "col-start-2 col-span-4" ] [ text pre ]
                            , div [ class "p-2 col-start-2 col-span-4" ] [ text data.state.userAnswer ]
                            , p [ class "col-start-2 col-span-4" ] [ text post ]
                            , div [ class "col-start-2 col-span-4" ] <| View.shuffledOptions data.state data.feedback task.radioMsg trial task.optionsOrder
                            , div [ class "col-start-2 col-span-4" ] <|
                                if data.feedback then
                                    [ text <|
                                        if data.state.userAnswer == trial.target then
                                            interpolate info.feedback_correct [ trial.target, trial.definition ]

                                        else
                                            interpolate info.feedback_incorrect [ trial.target, trial.definition ]
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
                        ]

                Nothing ->
                    div []
                        [ text "Intro is over"
                        , View.button
                            { message = task.startMainMsg data.mainTrials
                            , isDisabled = False
                            , txt = "Start"
                            }
                        ]

        ( PsychTask.IntroOver, Just info ) ->
            text "L'entrainement est fini"

        ( PsychTask.Main data, Just info ) ->
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
                    div []
                        [ text pre
                        , div [ class "text-lgz" ] [ text data.state.userAnswer ]
                        , text post
                        , div [] <| View.shuffledOptions data.state data.feedback task.radioMsg trial task.optionsOrder
                        , div [ class "col-start-2 col-span-4" ] <|
                            if data.feedback then
                                [ text <|
                                    if data.state.userAnswer == trial.target then
                                        interpolate info.feedback_correct [ trial.target, trial.definition ]

                                    else
                                        interpolate info.feedback_incorrect [ trial.target, trial.definition ]
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
                    text info.end

        ( PsychTask.Over, Just info ) ->
            text "I'm over"


type CU1Msg
    = UserClickedNextTrial
    | UserClickedToggleFeedback
    | UserClickedRadioButton String
    | ServerRespondedWith (Result Http.Error (List Trial))
    | UserClickedStartIntro (List Trial)
    | UserClickedStartMain (List Trial)


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
                |> required "Word_Text" string
                |> required "Distractor_1_CU_Lvl1" string
                |> required "Distractor_2_CU_Lvl1" string
                |> required "Distractor_3_CU_Lvl1" string
                |> required "Definition" string
                |> Data.decodeBool "isTraining"
    in
    Data.decodeRecords decoder


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
