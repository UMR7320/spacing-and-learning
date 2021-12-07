module Session2.CU2 exposing (..)

import Data
import Delay
import Dict
import ExperimentInfo
import Html.Styled as Html exposing (div, span, text)
import Html.Styled.Attributes exposing (class)
import Http exposing (Error)
import Json.Decode as Decode exposing (Decoder, string)
import Json.Decode.Pipeline exposing (..)
import Logic
import Ports
import Progressbar exposing (progressBar)
import Random
import Random.List
import Session1.Meaning exposing (Msg(..))
import Session3.CU3 exposing (Msg(..))
import View



-- MODEL


type Step
    = Listening Ntimes
    | Answering


type alias Ntimes =
    Int


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
    , speakerName : String
    , responseType : ResponseType
    , isTraining : Bool
    }


type ResponseType
    = Speech
    | Thought


type alias State =
    { userAnswer : String
    , step : Step
    }


initState : State
initState =
    State "" (Listening 3)


defaultTrial : Trial
defaultTrial =
    Trial "defaultTrial" "defaultTrial" (Data.AudioFile "" "") "defautcontext" "defaulttarget" "defautdis1" "defaultdis2" "defaultdis3" "defaultfeedback" "defaultName" Speech False


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


view exp optionsOrder =
    case exp of
        Logic.NotStarted ->
            div [] [ text "experiment did not start yet" ]

        Logic.Loading ->
            View.loading

        Logic.Err reason ->
            div [] [ text reason ]

        Logic.Running Logic.Instructions data ->
            div [] [ View.instructions data.infos UserClickedStartTraining ]

        Logic.Running Logic.Training data ->
            viewTrialOrEnd optionsOrder data (View.introToMain (UserClickedStartMain data.mainTrials data.infos))

        Logic.Running Logic.Main data ->
            viewTrialOrEnd optionsOrder data (View.end data.infos.end UserClickedSaveData "../post-tests/cw?session=S2")


viewTrialOrEnd optionsOrder data endView =
    case data.current of
        Just trial ->
            viewTrial optionsOrder data trial

        Nothing ->
            endView


viewTrial optionsOrder { mainTrials, current, state, feedback } trial =
    case state.step of
        Listening nTimes ->
            div []
                [ div [ class "context-understanding-2" ]
                    [ div [ class "p-4 bg-gray-200 rounded-lg context" ] [ View.fromMarkdown trial.context ]
                    , div [ class "with-thought-bubble" ]
                        [ div [ class "avatar-with-name" ]
                            [ if trial.responseType == Speech then
                                div [ class "text-4xl" ] [ text "😐" ]

                              else
                                div [ class "text-4xl" ] [ text "\u{1F914}" ]
                            , text (trial.speakerName ++ " ")
                            ]
                        , if trial.responseType == Speech then
                            div [ class "speech-bubble" ] []

                          else
                            div [ class "thought-bubble" ] []
                        ]
                    , div []
                        [ if nTimes == 3 then
                            View.audioButton UserClickedAudio trial.audioSentence.url "Listen"

                          else if nTimes == 2 then
                            View.audioButton UserClickedAudio trial.audioSentence.url "Listen again?"

                          else if nTimes == 1 then
                            View.audioButton UserClickedAudio trial.audioSentence.url "Listen for the last time?"

                          else
                            text ""
                        ]
                    ]
                , View.button
                    { isDisabled =
                        nTimes == 3
                    , message = UserClickedStartAnswering
                    , txt = "Now choose the best description"
                    }
                ]

        Answering ->
            div [ class "flex flex-col items-center" ]
                [ div [] <| View.shuffledOptions state feedback UserClickedRadioButton trial optionsOrder
                , View.genericSingleChoiceFeedback
                    { isVisible = feedback
                    , userAnswer = state.userAnswer
                    , target = trial.target
                    , feedback_Correct = ( trial.feedback, [] )
                    , feedback_Incorrect = ( trial.feedback, [] )
                    , button = View.navigationButton UserClickedToggleFeedback UserClickedNextTrial feedback state.userAnswer
                    }
                ]


audioButton trial nTimes =
    div [ class "" ]
        [ if nTimes == 3 then
            View.audioButton UserClickedAudio trial.audioSentence.url "Listen"

          else if nTimes == 2 then
            View.audioButton UserClickedAudio trial.audioSentence.url "Listen again?"

          else if nTimes == 1 then
            View.audioButton UserClickedAudio trial.audioSentence.url "Listen for the last time?"

          else
            div [] []
        ]


smallAudio txt =
    div [] [ text txt ]



-- UPDATE


type CU2Msg
    = UserClickedNextTrial
    | UserClickedToggleFeedback
    | UserClickedRadioButton String
    | UserClickedStartMain (List Trial) ExperimentInfo.Task
    | UserClickedSaveData
    | ServerRespondedWithLastRecords (Result Http.Error (List ()))
    | UserClickedAudio String
    | RuntimeShuffledOptionsOrder (List Int)
    | UserClickedStartTraining
    | UserClickedStartAnswering


update msg model =
    let
        prevState =
            Logic.getState model.cuLvl2 |> Maybe.withDefault initState
    in
    case msg of
        UserClickedNextTrial ->
            ( { model | cuLvl2 = Logic.next initState model.cuLvl2 }, Random.generate RuntimeShuffledOptionsOrder (Random.List.shuffle model.optionsOrder) )

        UserClickedToggleFeedback ->
            ( { model | cuLvl2 = Logic.toggle model.cuLvl2 }, Cmd.none )

        UserClickedRadioButton newChoice ->
            ( { model | cuLvl2 = Logic.update { prevState | userAnswer = newChoice } model.cuLvl2 }, Cmd.none )

        UserClickedStartMain _ _ ->
            ( { model | cuLvl2 = Logic.startMain model.cuLvl2 initState }, Cmd.none )

        ServerRespondedWithLastRecords _ ->
            ( model, Cmd.none )

        UserClickedSaveData ->
            let
                responseHandler =
                    ServerRespondedWithLastRecords
            in
            ( model, Logic.saveData responseHandler model.user taskId model.cuLvl2 )

        UserClickedAudio url ->
            ( { model | cuLvl2 = Logic.update { prevState | step = decrement prevState.step } model.cuLvl2 }
            , if prevState.step /= Listening 0 then
                Ports.playAudio url

              else
                Delay.after 0 UserClickedStartAnswering
            )

        RuntimeShuffledOptionsOrder ls ->
            ( { model | optionsOrder = ls }, Cmd.none )

        UserClickedStartTraining ->
            ( { model | cuLvl2 = Logic.startTraining model.cuLvl2 }, Cmd.none )

        UserClickedStartAnswering ->
            ( { model | cuLvl2 = Logic.update { prevState | step = Answering } model.cuLvl2 }, Cmd.none )


decrement : Step -> Step
decrement step =
    case step of
        Listening nTimes ->
            Listening (nTimes - 1)

        _ ->
            Answering



-- HTTP


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
                |> required "SpeakerName" string
                |> custom
                    (Decode.field "ResponseType" string
                        |> Decode.andThen
                            (\response ->
                                if response == "Speech" then
                                    Decode.succeed Speech

                                else
                                    Decode.succeed Thought
                            )
                    )
                |> optional "isTraining" Decode.bool False
    in
    Data.decodeRecords decoder



-- INTERNALS


taskId =
    "recwxsmowpB18bpLj"
