module Experiment.Translation exposing (..)

import Browser
import Css exposing (visibility)
import Data exposing (decodeRecords)
import Experiment.Experiment as E
import Experiment.Synonym as Synonym
import ExperimentInfo
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
import Progressbar
import PsychTask
import String.Interpolate exposing (interpolate)
import Url exposing (Url)
import Url.Builder
import View



--getTrialsFromServer : Decodera -> b


getTrialsFromServer : (Result Http.Error (List Trial) -> msg) -> Cmd msg
getTrialsFromServer msgHandler =
    Data.getTrialsFromServer_ "input" "Translation" msgHandler decodeTrials



-- Translation


type Msg
    = UserClickedNextTrial
    | UserClickedToggleFeedback
    | UserClickedRadioButton String
    | ServerRespondedWith (Result Http.Error (List Trial))
    | UserClickedStartIntro (List Trial)
    | UserClickedStartMain (List Trial)


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


viewQuestion word trialn =
    h3 []
        [ p []
            [ text <| String.fromInt (trialn + 1) ++ ". "
            , span [ class "italic" ] [ text word ]
            ]
        ]


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
                |> Data.decodeBool "isTraining"
    in
    Data.decodeRecords decoder


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

        ( PsychTask.Intr data, Just info ) ->
            case data.current of
                Just trial ->
                    View.viewTraining info.instructions
                        [ p [ class "col-start-2 col-span-4" ] [ viewQuestion trial.word (List.length data.history) ]
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
                                [ text <|
                                    if data.state.userAnswer == trial.target then
                                        interpolate info.feedback_correct [ trial.target ]

                                    else
                                        interpolate info.feedback_incorrect [ trial.target ]
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
                    div []
                        [ View.button
                            { message = task.startMainMsg data.mainTrials
                            , isDisabled = False
                            , txt = "Start"
                            }
                        ]

        ( PsychTask.IntroOver, Just info ) ->
            div [] []

        ( PsychTask.Main data, Just info ) ->
            case data.current of
                Just trial ->
                    div []
                        [ p [ class "col-start-2 col-span-4" ] [ viewQuestion trial.word (List.length data.history) ]
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
                                [ text <|
                                    if data.state.userAnswer == trial.target then
                                        interpolate info.feedback_correct [ trial.target ]

                                    else
                                        interpolate info.feedback_incorrect [ trial.target ]
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
                    div [] []

        ( PsychTask.Over, Just info ) ->
            div [] []

        ( PsychTask.NotStartedYet, Just info ) ->
            div [] []
