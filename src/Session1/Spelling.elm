module Session1.Spelling exposing (..)

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
import Ports
import Progressbar
import Random
import Random.List
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


updateWhenNextTrial model shuffleOptionsMsg =
    ( { model
        | spellingLvl1 =
            Logic.next initState model.spellingLvl1

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
        , div [ class "text-lg text-green-500 font-bold pb-2" ] [ span [] [ text "Practice here !" ] ]
        ]


trainingBox =
    div [ class "container w-full h-full border-4 border-green-500 border-rounded-lg border-dashed flex items-center justify-center flex-col" ]


viewTask data currentTrial ordoredOptions =
    [ View.audioButton UserClickedPlayAudio currentTrial.audio.url "word"
    , div
        [ class "pt-6 center-items justify-center max-w-xl w-full mt-6 ", disabled data.feedback ]
        [ fieldset
            []
            ordoredOptions
        , View.genericSingleChoiceFeedback
            { isVisible = data.feedback
            , userAnswer = data.state.userAnswer
            , target = currentTrial.target
            , feedback_Correct = ( data.infos.feedback_correct, [ View.bold currentTrial.target ] )
            , feedback_Incorrect = ( data.infos.feedback_incorrect, [ View.bold currentTrial.target ] )
            , button = View.navigationButton UserClickedFeedback UserClickedNextTrial data.feedback
            }
        ]
    ]


update msg model =
    let
        taskId =
            "recJOpE5pMTCHJOSV"

        currentSpellingState =
            Logic.getState model.spellingLvl1
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
            case currentSpellingState of
                Just prevState ->
                    ( { model
                        | spellingLvl1 =
                            Logic.update { prevState | userAnswer = newChoice } model.spellingLvl1
                      }
                    , Cmd.none
                    )

                Nothing ->
                    ( model, Cmd.none )

        UserClickedNextTrial ->
            ( { model | spellingLvl1 = Logic.next initState model.spellingLvl1 }, Cmd.none )

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
            ( model, Ports.playAudio url )

        UserClickedStartTraining ->
            ( { model | spellingLvl1 = Logic.startTraining model.spellingLvl1 }, Cmd.none )


view :
    Logic.Task Trial State
    -> List Int
    -> Html Msg
view exp optionsOrder =
    case exp of
        Logic.Loading ->
            text "Loading..."

        Logic.NotStarted ->
            text "I'm not started yet"

        Logic.Running Logic.Instructions data ->
            div [ class "flex flex-col items-center" ] [ View.instructions data.infos.instructions UserClickedStartTraining ]

        Logic.Err reason ->
            text <| "Error: " ++ reason

        Logic.Running Logic.Training ({ trainingTrials, mainTrials, current, state, feedback, history, infos } as data) ->
            case current of
                Just x ->
                    let
                        option id =
                            View.radio
                                id
                                (state.userAnswer == id)
                                (isCorrect id)
                                feedback
                                (UserClickedRadioButton id)

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
                    div [] <|
                        View.trainingWheelsGeneric
                            (List.length history)
                            data.infos.trainingWheel
                            [ View.bold x.target ]
                            :: viewTask data x ordoredOptions

                Nothing ->
                    View.introToMain UserClickedStartMainloop

        Logic.Running Logic.Main ({ mainTrials, current, state, feedback, history, infos } as data) ->
            case current of
                Just trial ->
                    let
                        option id =
                            View.radio
                                id
                                (state.userAnswer == id)
                                (isCorrect id)
                                feedback
                                (UserClickedRadioButton id)

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
                        [ div [ class "mr-8 w-full max-w-xl" ] <|
                            Progressbar.progressBar history mainTrials
                                :: viewTask
                                    data
                                    trial
                                    ordoredOptions
                        ]

                Nothing ->
                    View.end infos.end UserClickedSavedData "context-understanding"


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
