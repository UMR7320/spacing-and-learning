module Session1.SpellingLvl1 exposing (..)

import Array
import Data exposing (decodeRecords)
import Dict
import ExperimentInfo
import Html.Styled exposing (Html, div, fieldset, h1, h2, h3, p, pre, span, text)
import Html.Styled.Attributes exposing (class, disabled)
import Http
import Json.Decode as Decode
import Json.Decode.Pipeline exposing (..)
import Logic exposing (saveData)
import Progressbar
import Random
import Random.List
import Session1.CU1 exposing (CU1Msg(..))
import Session3.Synonym exposing (Msg(..))
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


type Msg
    = UserClickedNextTrial
    | UserClickedFeedback
    | UserClickedRadioButton String
    | UserClickedStartMainloop (List Trial) ExperimentInfo.Task
    | UserClickedSavedData
    | ServerRespondedWithLastRecords (Result Http.Error (List String))


updateWhenNextTrial model shuffleOptionsMsg =
    ( { model
        | spellingLvl1 =
            Logic.next initState model.spellingLvl1

        -- ICI GROS BUG À VENIR, il faut reset uniquement la réponse du volontaire
      }
    , Random.generate shuffleOptionsMsg (Random.List.shuffle model.optionsOrder)
    )


viewInstructions =
    div [ class "flex flex-col" ]
        [ h2 [ class "font-bold" ] [ text "Instructions" ]
        , p [ class "pt-8 pb-8 font-medium" ]
            [ pre [] [ text "Focus on the word in the box in each sentence.\u{200B}\nIt’s a synonym for one of the words you are learning.\u{200B}\nClick on the word in the box to type your synonym." ]
            ]
        , div [ class "text-lg text-green-500 font-bold pb-2" ] [ span [] [ text "Practice here !" ] ]
        ]


trainingBox =
    div [ class "container w-full h-full border-4 border-green-500 border-rounded-lg border-dashed flex items-center justify-center flex-col" ]


view :
    Logic.Task Trial State
    -> List Int
    ->
        { toggleFeedbackMsg : msg
        , radioMsg : String -> msg
        , nextTrialMsg : msg
        , startMainloopMsg : List Trial -> ExperimentInfo.Task -> msg
        , playAudio : String -> msg
        , saveDataMsg : msg
        }
    -> Html msg
view exp optionsOrder { radioMsg, nextTrialMsg, toggleFeedbackMsg, startMainloopMsg, playAudio, saveDataMsg } =
    case exp of
        Logic.Loading ->
            text "Loading..."

        Logic.NotStarted ->
            text "I'm not started yet"

        Logic.Err reason ->
            text reason

        Logic.Intr { trainingTrials, mainTrials, current, state, feedback, history, infos } ->
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
                            [ View.trainingWheelsGeneric (List.length history) infos.trainingWheel [ x.target ]
                            , View.play playAudio x.audio.url
                            , div
                                [ class "pt-6 center-items justify-center max-w-xl w-full mt-6 ", disabled feedback ]
                                [ fieldset
                                    []
                                    ordoredOptions
                                ]
                            , View.genericSingleChoiceFeedback
                                { isVisible = feedback
                                , userAnswer = state.userAnswer
                                , target = x.target
                                , feedback_Correct = ( infos.feedback_correct, [ x.target ] )
                                , feedback_Incorrect = ( infos.feedback_incorrect, [ x.target ] )
                                , button = View.navigationButton toggleFeedbackMsg nextTrialMsg feedback
                                }
                            ]
                        ]

                Nothing ->
                    View.introToMain (startMainloopMsg mainTrials infos)

        Logic.Main { mainTrials, current, state, feedback, history, infos } ->
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
                    div [ class "container w-full flex flex-col justify-center items-center" ]
                        [ div [ class "mr-8 w-full max-w-xl" ]
                            [ Progressbar.progressBar (View.pct trialn (mainTrials ++ List.map Tuple.first history))
                            , View.play playAudio trial.audio.url
                            , div
                                [ class "pt-6 max-w-xl ", disabled feedback ]
                                [ fieldset
                                    []
                                    ordoredOptions
                                ]

                            --, View.navigationButton toggleFeedbackMsg nextTrialMsg feedback
                            , View.genericSingleChoiceFeedback
                                { isVisible = feedback
                                , userAnswer = state.userAnswer
                                , target = trial.target
                                , feedback_Correct = ( infos.feedback_correct, [ trial.target ] )
                                , feedback_Incorrect = ( infos.feedback_incorrect, [ trial.target ] )
                                , button = View.navigationButton toggleFeedbackMsg nextTrialMsg feedback
                                }
                            ]
                        ]

                Nothing ->
                    View.end infos.end saveDataMsg


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


start info trials =
    let
        id =
            "recJOpE5pMTCHJOSV"

        relatedInfos =
            Dict.get id (ExperimentInfo.toDict info) |> Result.fromMaybe ("I couldn't fetch the value associated with: " ++ id)
    in
    Logic.startIntro relatedInfos
        (List.filter (\datum -> datum.isTraining) trials)
        (List.filter (\datum -> not datum.isTraining) trials)
        initState


getTrialsFromServer : (Result Http.Error (List Trial) -> msg) -> Cmd msg
getTrialsFromServer callbackMsg =
    Data.getTrialsFromServer_ "input" "Meaning" callbackMsg decodeTrials


initState : State
initState =
    State "DefaultTrialUID" "DefaultUserUID" ""
