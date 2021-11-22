module Session1.Meaning exposing (..)

import Data exposing (decodeRecords)
import Dict
import ExperimentInfo
import Html.Styled
    exposing
        ( Html
        , div
        , h1
        , h3
        , p
        , span
        , text
        )
import Html.Styled.Attributes exposing (class, disabled)
import Http
import Json.Decode as Decode exposing (Decoder, bool, string)
import Json.Decode.Pipeline exposing (..)
import Json.Encode as Encode
import Logic
import Progressbar
import Random
import Random.List
import Session2.Translation exposing (Msg(..))
import String.Interpolate exposing (interpolate)
import View



-- MODEL


type alias Meaning =
    Logic.Task Trial State


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


initState : State
initState =
    State "DefaultTrialUID" ""



-- VIEW


view : { task : Logic.Task Trial State, optionsOrder : List comparable } -> Html Msg
view task =
    case task.task of
        Logic.Loading ->
            View.loading

        Logic.Err reason ->
            div [] [ text reason ]

        Logic.Running Logic.Training data ->
            case data.current of
                Just trial ->
                    div [ class "flex flex-col items-center" ]
                        [ p [] [ View.trainingWheelsGeneric (List.length data.history) data.infos.trainingWheel [ View.bold trial.writtenWord, View.bold trial.target ] ]
                        , p [] [ viewQuestion ("to " ++ trial.writtenWord) (List.length data.history) ]
                        , div
                            [ class "w-full ", disabled data.feedback ]
                          <|
                            View.shuffledOptions
                                data.state
                                data.feedback
                                UserClickedRadioButton
                                trial
                                task.optionsOrder
                        , View.genericSingleChoiceFeedback
                            { isVisible = data.feedback
                            , userAnswer = data.state.userAnswer
                            , target = trial.target
                            , feedback_Correct = ( trial.feedbackIncorrect, [] )
                            , feedback_Incorrect = ( trial.feedbackCorrect, [] )
                            , button = View.navigationButton UserClickedToggleFeedback UserClickedNextTrial data.feedback data.state.userAnswer
                            }
                        ]

                Nothing ->
                    View.introToMain UserClickedStartMain

        Logic.Running Logic.Main data ->
            case data.current of
                Just trial ->
                    div [ class "flex flex-col items-center " ]
                        [ viewQuestion ("to " ++ trial.writtenWord) (List.length data.history)
                        , div
                            [ class " center-items justify-center w-full mt-6 ", disabled data.feedback ]
                          <|
                            View.shuffledOptions
                                data.state
                                data.feedback
                                UserClickedRadioButton
                                trial
                                task.optionsOrder
                        , View.genericSingleChoiceFeedback
                            { isVisible = data.feedback
                            , userAnswer = data.state.userAnswer
                            , target = trial.target
                            , feedback_Correct = ( trial.feedbackIncorrect, [] )
                            , feedback_Incorrect = ( trial.feedbackCorrect, [] )
                            , button = View.navigationButton UserClickedToggleFeedback UserClickedNextTrial data.feedback data.state.userAnswer
                            }
                        ]

                Nothing ->
                    View.end data.infos.end SaveDataMsg "spelling"

        Logic.NotStarted ->
            div [] [ text "I did not start yet." ]

        Logic.Running Logic.Instructions data ->
            View.instructions data.infos UserClickedStartTraining


viewQuestion word trialn =
    div [ class "text-3xl font-bold italic my-6" ] [ text word ]



-- UPDATE


type Msg
    = UserClickedNextTrial
    | UserClickedToggleFeedback
    | UserClickedRadioButton String
    | UserClickedStartMain
    | SaveDataMsg
    | UserClickedStartTraining
    | RuntimeShuffledOptionsOrder (List Int)
    | HistoryWasSaved (Result Http.Error String)


update msg model =
    case msg of
        UserClickedNextTrial ->
            ( { model
                | meaning = Logic.next initState model.meaning
              }
            , Cmd.batch
                [ Random.generate RuntimeShuffledOptionsOrder (Random.List.shuffle model.optionsOrder)
                , saveData model
                ]
            )

        UserClickedToggleFeedback ->
            ( { model | meaning = Logic.toggle model.meaning }, Cmd.none )

        UserClickedRadioButton newChoice ->
            ( { model | meaning = Logic.update { uid = "", userAnswer = newChoice } model.meaning }, Cmd.none )

        UserClickedStartMain ->
            ( { model | meaning = Logic.startMain model.meaning initState }, Cmd.none )

        -- data is now saved after each "trial", so this does nothing and shoud be removed
        SaveDataMsg ->
            ( model, Cmd.none )

        HistoryWasSaved _ ->
            ( model, Cmd.none )

        UserClickedStartTraining ->
            ( { model | meaning = Logic.startTraining model.meaning }, Cmd.none )

        RuntimeShuffledOptionsOrder newOrder ->
            ( { model | optionsOrder = newOrder }, Cmd.none )



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
        , resolver = Http.stringResolver <| Data.handleJsonResponse <| decodeMeaningInput
        , timeout = Just 5000
        }


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
                |> optional "isTraining" bool False
    in
    decodeRecords decoder


getTrialsFromServer : (Result Http.Error (List Trial) -> msg) -> Cmd msg
getTrialsFromServer callbackMsg =
    Data.getTrialsFromServer_ "input" "Meaning" callbackMsg decodeMeaningInput


saveData model =
    let
        history =
            Logic.getHistory model.meaning
                |> List.filter (\( trial, _ ) -> not trial.isTraining)

        userId =
            model.user |> Maybe.withDefault "recd18l2IBRQNI05y"

        payload =
            updateHistoryEncoder userId history
    in
    Http.request
        { method = "PATCH"
        , headers = []
        , url = Data.buildQuery { app = Data.apps.spacing, base = "users", view_ = "Session1_output" }
        , body = Http.jsonBody payload
        , expect = Http.expectJson HistoryWasSaved (Decode.succeed "OK")
        , timeout = Nothing
        , tracker = Nothing
        }


updateHistoryEncoder : String -> List ( Trial, State ) -> Encode.Value
updateHistoryEncoder userId history =
    -- The Netflify function that receives PATCH requests only works with arrays
    Encode.list
        (\_ ->
            Encode.object
                [ ( "id", Encode.string userId )
                , ( "fields", historyEncoder userId history )
                ]
        )
        [ ( userId, history ) ]


historyEncoder : String -> List ( Trial, State ) -> Encode.Value
historyEncoder userId history =
    Encode.object
        -- airtable does not support JSON columns, so we save giant JSON strings
        [ ( "Meaning1", Encode.string (Encode.encode 0 (Encode.list historyItemEncoder history)) )
        ]


historyItemEncoder : ( Trial, State ) -> Encode.Value
historyItemEncoder ( { uid }, { userAnswer } ) =
    Encode.object
        [ ( "trialUid", Encode.string uid )
        , ( "answser", Encode.string userAnswer )
        ]



-- INTERNALS


taskId =
    "rec9fDmVOpqDJktmQ"
