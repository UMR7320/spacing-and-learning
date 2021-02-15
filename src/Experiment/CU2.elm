module Experiment.CU2 exposing (..)

import Data
import ExperimentInfo
import Html.Styled as Html exposing (Html, div, fromUnstyled, span, text)
import Html.Styled.Attributes exposing (class)
import Html.Styled.Events
import Http exposing (Error)
import Icons
import Json.Decode as Decode exposing (Decoder, string)
import Json.Decode.Pipeline exposing (..)
import Markdown
import PsychTask
import View


view exp infos optionsOrder { userClickedAudio, radioMsg, toggleFeedback, nextTrialMsg, startMainMsg } =
    case ( exp, infos ) of
        ( _, Nothing ) ->
            div [] [ text "Error : I can't find the task's infos" ]

        ( PsychTask.NotStartedYet, Just info ) ->
            div [] [ text "experiment did not start yet" ]

        ( PsychTask.Intr { trainingTrials, mainTrials, current, state, feedback, history }, Just info ) ->
            case current of
                Just trial ->
                    div []
                        [ View.viewTraining info.instructions
                            [ div [ class "col-start-2 col-span-4 pt-8 pb-8" ]
                                [ span [] [ text trial.context ]
                                , div [ class "h-8 w-8 pt-4", Html.Styled.Events.onClick (userClickedAudio trial.audioSentence.url) ] [ fromUnstyled <| Icons.music ]
                                ]
                            , div [ class "col-start-2 col-span-4" ] <| View.shuffledOptions state feedback radioMsg trial optionsOrder
                            , div [ class "col-start-2 col-span-4 pb-8" ] <|
                                if feedback then
                                    [ View.fromMarkdown trial.feedback
                                    , View.button
                                        { message = nextTrialMsg
                                        , isDisabled = False
                                        , txt = "Next Training Item"
                                        }
                                    ]

                                else
                                    [ View.button
                                        { message = toggleFeedback
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
                            { message = startMainMsg mainTrials
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
                        [ View.fromMarkdown info.instructions_short
                        , span [] [ text trial.context ]
                        , div [ class "h-12 w-12", Html.Styled.Events.onClick (userClickedAudio trial.audioSentence.url) ] [ fromUnstyled <| Icons.music ]
                        , div [] <| View.shuffledOptions state feedback radioMsg trial optionsOrder
                        , if feedback then
                            div []
                                [ fromUnstyled <| Markdown.toHtml [] trial.feedback
                                , View.button
                                    { message = nextTrialMsg
                                    , isDisabled = False
                                    , txt = "Next Item"
                                    }
                                ]

                          else
                            View.button
                                { message = toggleFeedback
                                , isDisabled = False
                                , txt = "Check my answer"
                                }
                        ]

                Nothing ->
                    text info.end

        ( PsychTask.Over, Just info ) ->
            text "I'm over"


type CU2Msg
    = UserClickedNextTrial
    | UserClickedToggleFeedback
    | UserClickedRadioButton String
    | ServerRespondedWith (Result Http.Error (List Trial))
    | UserClickedStartIntro (List Trial)
    | UserClickedStartMain (List Trial)


getTrialsFromServer : (Result Error (List Trial) -> msg) -> Cmd msg
getTrialsFromServer msgHandler =
    Data.getTrialsFromServer_ "input" "SpellingLvl2" msgHandler decodeTranslationInput


decodeTranslationInput : Decoder (List Trial)
decodeTranslationInput =
    let
        decoder =
            Decode.succeed Trial
                |> required "UID" string
                |> required "Word_Text" string
                |> optional "Audio_Understanding" Data.decodeAudioFiles (Data.AudioFile "" "")
                |> required "CU_Lvl1_Context" string
                |> required "CU_Lvl2_target" string
                |> required "CU_Lvl2_Distractor_1" string
                |> required "CU_Lvl2_Distractor_2" string
                |> required "CU_Lvl2_Distractor_3" string
                |> required "Feedback_CU_Lvl2" string
                |> Data.decodeBool "isTraining"
    in
    Data.decodeRecords decoder


initState : State
initState =
    State "DefaultUid" ""


defaultTrial : Trial
defaultTrial =
    Trial "defaultTrial" "defaultTrial" (Data.AudioFile "" "") "defautcontext" "defaulttarget" "defautdis1" "defaultdis2" "defaultdis3" "defaultfeedback" False


type alias Trial =
    { uid : String
    , writtenWord : String
    , audioSentence : Data.AudioFile
    , context : String
    , target : String
    , distractor1 : String
    , distractor2 : String
    , distractor3 : String
    , feedback : String
    , isTraining : Bool
    }


type alias State =
    { uid : String
    , userAnswer : String
    }
