module Session1.Spelling exposing (..)

import Data exposing (decodeRecords)
import Dict
import ExperimentInfo
import Html.Styled exposing (Html, div, fieldset, h2, p, pre, span, text)
import Html.Styled.Attributes exposing (class, disabled)
import Http
import Json.Decode as Decode
import Json.Decode.Pipeline exposing (..)
import Logic
import Ports
import Progressbar
import Random
import Random.List
import Session2.CU2 exposing (Step(..))
import Session3.Synonym exposing (Msg(..))
import View


type alias Spelling =
    Logic.Task Trial State


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
    | UserClickedStartMainloop
    | UserClickedSavedData
    | ServerRespondedWithLastRecords (Result Http.Error (List ()))
    | UserClickedPlayAudio String
    | UserClickedStartTraining
    | AudioEnded { eventType : String, name : String, timestamp : Int }


updateWhenNextTrial model shuffleOptionsMsg =
    ( { model
        | spellingLvl1 =
            Logic.next iniState model.spellingLvl1

        -- ICI GROS BUG À VENIR, il faut reset uniquement la réponse du volontaire
      }
    , Random.generate shuffleOptionsMsg (Random.List.shuffle model.optionsOrder)
    )


viewInstructions txt =
    div [ class "flex flex-col" ]
        [ h2 [ class "font-bold" ] [ text "Instructions" ]
        , p [ class "pt-8 pb-8 font-medium" ]
            [ pre [] [ text txt ]
            ]
        , div [ class "text-lg text-green-500 font-bold pb-2" ] [ span [] [ text "Practice here!" ] ]
        ]


trainingBox =
    div [ class "container w-full h-full border-4 border-green-500 border-rounded-lg border-dashed flex items-center justify-center flex-col" ]


viewTask data currentTrial optionsOrder =
    [ viewAudioButton data.state.remainingListenings currentTrial.audio.url
    , div
        [ class "pt-6 center-items justify-center max-w-xl w-full mt-6 ", disabled data.feedback ]
        [ if data.state.step == Answering then
            div [] <| View.shuffledOptions data.state data.feedback UserClickedRadioButton currentTrial optionsOrder

          else
            div [] []
        , View.genericSingleChoiceFeedback
            { isVisible = data.feedback
            , userAnswer = data.state.userAnswer
            , target = currentTrial.target
            , feedback_Correct = ( data.infos.feedback_correct, [ View.bold currentTrial.target ] )
            , feedback_Incorrect = ( data.infos.feedback_incorrect, [ View.bold currentTrial.target ] )
            , button = View.navigationButton UserClickedFeedback UserClickedNextTrial data.feedback data.state.userAnswer
            }
        ]
    ]


update msg model =
    let
        taskId =
            "recJOpE5pMTCHJOSV"

        currentSpellingState =
            Logic.getState model.spellingLvl1 |> Maybe.withDefault iniState
    in
    case msg of
        UserClickedFeedback ->
            ( { model
                | spellingLvl1 =
                    model.spellingLvl1
                        |> Logic.toggle
              }
            , Cmd.none
            )

        UserClickedRadioButton newChoice ->
            ( { model
                | spellingLvl1 =
                    Logic.update { currentSpellingState | userAnswer = newChoice } model.spellingLvl1
              }
            , Cmd.none
            )

        UserClickedNextTrial ->
            ( { model | spellingLvl1 = Logic.next iniState model.spellingLvl1 }, Cmd.none )

        UserClickedStartMainloop ->
            ( { model | spellingLvl1 = Logic.startMain model.spellingLvl1 iniState }, Cmd.none )

        UserClickedSavedData ->
            let
                responseHandler =
                    ServerRespondedWithLastRecords
            in
            ( model, Logic.saveData responseHandler model.user taskId model.spellingLvl1 )

        ServerRespondedWithLastRecords _ ->
            ( model, Cmd.none )

        UserClickedPlayAudio url ->
            ( { model | spellingLvl1 = Logic.update { currentSpellingState | remainingListenings = currentSpellingState.remainingListenings - 1 } model.spellingLvl1 }, Ports.playAudio url )

        UserClickedStartTraining ->
            ( { model | spellingLvl1 = Logic.startTraining model.spellingLvl1 }, Cmd.none )

        AudioEnded { eventType } ->
            case eventType of
                "SoundEnded" ->
                    ( { model | spellingLvl1 = Logic.update { currentSpellingState | step = Answering } model.spellingLvl1 }, Cmd.none )

                _ ->
                    ( model, Cmd.none )


view :
    Logic.Task Trial State
    -> List Int
    -> Html Msg
view exp optionsOrder =
    case exp of
        Logic.Loading ->
            View.loading

        Logic.NotStarted ->
            text "I'm not started yet"

        Logic.Running Logic.Instructions data ->
            div [ class "flex flex-col items-center" ] [ View.instructions data.infos.instructions UserClickedStartTraining ]

        Logic.Err reason ->
            text <| "Error: " ++ reason

        Logic.Running Logic.Training ({ trainingTrials, mainTrials, current, state, feedback, history, infos } as data) ->
            case current of
                Just x ->
                    div [ class "container w-full flex flex-col justify-center items-center" ] <|
                        View.trainingWheelsGeneric
                            (List.length history)
                            data.infos.trainingWheel
                            [ View.bold x.target ]
                            :: viewTask data x optionsOrder

                Nothing ->
                    View.introToMain UserClickedStartMainloop

        Logic.Running Logic.Main ({ mainTrials, current, state, feedback, history, infos } as data) ->
            case current of
                Just trial ->
                    div [ class "flex flex-col justify-center items-center" ] <|
                        View.tooltip data.infos.instructions_short
                            :: Progressbar.progressBar history mainTrials
                            :: viewTask
                                data
                                trial
                                optionsOrder

                Nothing ->
                    View.end infos.end UserClickedSavedData "context-understanding"


subscriptions model =
    case model.spellingLvl1 of
        Logic.Running _ { state } ->
            case state.step of
                ListeningFirstTime ->
                    Sub.batch [ Ports.audioEnded AudioEnded ]

                _ ->
                    Sub.none

        _ ->
            Sub.none


viewAudioButton nTimes url =
    case nTimes of
        3 ->
            View.audioButton UserClickedPlayAudio url "Listen"

        2 ->
            View.audioButton UserClickedPlayAudio url "Listen again?"

        1 ->
            View.audioButton UserClickedPlayAudio url "Listen for the last time?"

        _ ->
            div [] []


type alias State =
    { inputUid : String
    , userAnswer : String
    , remainingListenings : Int
    , step : Step
    }


type Step
    = ListeningFirstTime
    | Answering


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
                |> optional "isTraining" Decode.bool False
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
        iniState


getTrialsFromServer : (Result Http.Error (List Trial) -> msg) -> Cmd msg
getTrialsFromServer callbackMsg =
    Data.getTrialsFromServer_ "input" "Meaning" callbackMsg decodeTrials


iniState : State
iniState =
    State "DefaultTrialUID" "" 3 ListeningFirstTime
