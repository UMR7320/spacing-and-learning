module Experiment.SpellingLvl1 exposing (..)

import Array
import Data exposing (decodeRecords)
import Html.Styled exposing (Html, div, fieldset, h1, h2, h3, p, pre, span, text)
import Html.Styled.Attributes exposing (class, disabled)
import Http
import Json.Decode as Decode
import Json.Decode.Pipeline exposing (..)
import Progressbar
import PsychTask
import View


type alias Trial =
    { uid : String
    , target : String
    , distractor1 : String
    , distractor2 : String
    , distractor3 : String
    , isTraining : Bool
    , audio : Data.AudioFile
    }


viewInstructions =
    div [ class "flex flex-col" ]
        [ h2 [ class "font-bold" ] [ text "Instructions" ]
        , p [ class "pt-8 pb-8 font-medium" ]
            [ pre [] [ text "Focus on the word in the box in each sentence.\u{200B}\nIt’s a synonym for one of the words you are learning.\u{200B}\nClick on the word in the box to type your synonym." ]
            ]
        , div [ class "text-lg text-green-500 font-bold pb-2" ] [ span [] [ text "Practice here !" ] ]
        ]


trainingBox =
    div [ class "container w-full h-full border-4 border-green-500 border-rounded-lg border-dashed text-center object-center " ]


view :
    PsychTask.Task Trial State
    -> List Int
    ->
        { toggleFeedbackMsg : msg
        , radioMsg : String -> msg
        , nextTrialMsg : msg
        , startMainloopMsg : List Trial -> msg
        }
    -> Html msg
view exp optionsOrder { radioMsg, nextTrialMsg, toggleFeedbackMsg, startMainloopMsg } =
    case exp of
        PsychTask.Over ->
            text "I'm over"

        PsychTask.NotStartedYet ->
            text "I'm not started yet"

        PsychTask.Intr { trainingTrials, mainTrials, current, state, feedback, history } ->
            case current of
                Just x ->
                    let
                        option id =
                            View.radio
                                id
                                (state.userAnswer == id)
                                (isCorrect id)
                                feedback
                                (radioMsg id)

                        isCorrect optionN =
                            optionN == x.target

                        options =
                            [ option x.distractor1, option x.distractor2, option x.distractor3, option x.target ]

                        ordoredOptions =
                            options
                                |> List.map2 Tuple.pair optionsOrder
                                |> List.sortBy Tuple.first
                                |> List.map Tuple.second
                    in
                    div []
                        [ viewInstructions
                        , trainingBox
                            [ View.viewQuestion (List.length history) x "Choose the correct spelling for the word :"
                            , View.simpleAudioPlayer x.audio.url
                            , div
                                [ class "pt-6 max-w-xl ", disabled feedback ]
                                [ fieldset
                                    []
                                    ordoredOptions
                                ]
                            , View.button <|
                                if not feedback then
                                    { message = toggleFeedbackMsg
                                    , txt = "Is it correct?"
                                    , isDisabled = False
                                    }

                                else
                                    { message = nextTrialMsg
                                    , txt = "Next "
                                    , isDisabled = False
                                    }
                            , div [ class "mt-4 max-w-xl w-full" ] [ View.viewFeedbackInSingleChoice feedback state x ]
                            ]
                        ]

                Nothing ->
                    div []
                        [ text "Ready to start the real experiment?"
                        , View.button
                            { message = startMainloopMsg mainTrials
                            , txt = "Click here to start"
                            , isDisabled = False
                            }
                        ]

        PsychTask.IntroOver ->
            text "L'entrainement est fini"

        PsychTask.Main { mainTrials, current, state, feedback, history } ->
            case current of
                Just trial ->
                    let
                        option id =
                            View.radio
                                id
                                (state.userAnswer == id)
                                (isCorrect id)
                                feedback
                                (radioMsg id)

                        trialn =
                            List.length history + 1

                        isCorrect optionN =
                            optionN == trial.target

                        options =
                            [ option trial.distractor1, option trial.distractor2, option trial.distractor3, option trial.target ]

                        ordoredOptions =
                            options
                                |> List.map2 Tuple.pair optionsOrder
                                |> List.sortBy Tuple.first
                                |> List.map Tuple.second
                    in
                    div [ class "flex flex-wrap items-center" ]
                        [ div [ class "mr-8" ]
                            [ Progressbar.progressBar (View.pct trialn (mainTrials ++ List.map Tuple.first history))
                            , View.viewQuestion trialn trial "Choose the correct spelling for the word :"
                            , View.simpleAudioPlayer trial.audio.url
                            , div
                                [ class "pt-6 max-w-xl ", disabled feedback ]
                                [ fieldset
                                    []
                                    ordoredOptions
                                ]
                            , View.button <|
                                if not feedback then
                                    { message = toggleFeedbackMsg
                                    , txt = "Is it correct?"
                                    , isDisabled = String.isEmpty state.userAnswer
                                    }

                                else
                                    { message = nextTrialMsg
                                    , txt = "Next "
                                    , isDisabled = False
                                    }
                            , div [ class "mt-4 max-w-xl w-full" ] [ View.viewFeedbackInSingleChoice feedback state trial ]
                            ]
                        ]

                Nothing ->
                    text "Experiment is over"


type alias State =
    { inputUid : String
    , userUID : String
    , userAnswer : String
    }


iniState =
    { inputUid = ""
    , userUID = ""
    , userAnswer = ""
    }


decodeTrials : Decode.Decoder (List Trial)
decodeTrials =
    let
        stringToBoolDecoder : String -> Decode.Decoder Bool
        stringToBoolDecoder str =
            case str of
                "true" ->
                    Decode.succeed True

                _ ->
                    Decode.succeed False

        decoder =
            Decode.succeed Trial
                |> required "UID" Decode.string
                |> required "Word_Text" Decode.string
                |> required "Distractor_1_CCS" Decode.string
                |> required "Distractor_2_CCS" Decode.string
                |> required "Distractor_3_CCS" Decode.string
                |> custom (Decode.field "isTraining" Decode.string |> Decode.andThen stringToBoolDecoder)
                |> required "Word_Audio" Data.decodeAudioFiles
    in
    decodeRecords decoder


getTrialsFromServer : (Result Http.Error (List Trial) -> msg) -> Cmd msg
getTrialsFromServer callbackMsg =
    Data.getTrialsFromServer_ "input" "Meaning" callbackMsg decodeTrials


initState : State
initState =
    State "DefaultTrialUID" "DefaultUserUID" ""
