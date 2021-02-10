module Experiment.CU1 exposing (..)

import Data
import ExperimentInfo
import Html.Styled as Html exposing (Html, div, fromUnstyled, span, text)
import Html.Styled.Attributes exposing (class)
import Html.Styled.Events
import Http exposing (Error)
import Icons
import Json.Decode as Decode exposing (Decoder, string)
import Json.Decode.Pipeline exposing (..)
import PsychTask
import View



--view : { a | task : PsychTask.Task t s, optionsOrder : Maybe (List comparable), nextTrialMsg : f, userClickedAudio : String -> f, radioMsg : String -> f, toggleFeedbackMsg : f, startMainMsg : List t -> f, inputChangedMsg : String -> f } -> Html msg


view :
    { task : PsychTask.Task t s
    , infos : Maybe ExperimentInfo.Task
    , optionsOrder : Maybe (List comparable)
    , nextTrialMsg : msg
    , userClickedAudio : Maybe (String -> msg)
    , radioMsg : Maybe (String -> msg)
    , toggleFeedbackMsg : msg
    , startMainMsg : List t -> msg
    , inputChangedMsg : Maybe (String -> msg)
    }
    -> Html msg
view input =
    case ( input.task, input.infos ) of
        ( _, Nothing ) ->
            div [] [ text "Error : I can't find the task's infos" ]

        ( PsychTask.NotStartedYet, Just info ) ->
            div [] [ text "experiment did not start yet" ]

        ( PsychTask.Intr { trainingTrials, mainTrials, current, state, feedback, history }, Just info ) ->
            case current of
                Just trial ->
                    div []
                        [ text "je suis intro" ]

                Nothing ->
                    div []
                        [ text "Intro is over"
                        , View.button
                            { message = input.startMainMsg mainTrials
                            , isDisabled = False
                            , txt = "Start"
                            }
                        ]

        ( PsychTask.IntroOver, Just info ) ->
            text "L'entrainement est fini"

        ( PsychTask.Main { mainTrials, current, state, feedback, history }, Just info ) ->
            case current of
                Just trial ->
                    div []
                        []

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
                |> Data.decodeBool
    in
    Data.decodeRecords decoder


initState : State
initState =
    State "DefaultUid" ""


defaultTrial : Trial
defaultTrial =
    Trial "defaultuid" "defaulttarger" "defaultText" "distractor1" "distractor2" "distractor3" False


type alias Trial =
    { uid : String
    , text : String
    , target : String
    , distractor1 : String
    , distractor2 : String
    , distractor3 : String
    , isTraining : Bool
    }


type alias State =
    { uid : String
    , userAnswer : String
    }
