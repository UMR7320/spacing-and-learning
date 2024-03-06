module Session1.Meaning1 exposing (..)

import Activity exposing (Activity)
import ActivityInfo exposing (ActivityInfo, Session(..))
import Data exposing (decodeRecords)
import Html.Styled exposing (Html, div, p, text)
import Html.Styled.Attributes exposing (class, disabled)
import Http
import Json.Decode as Decode exposing (Decoder, bool, string)
import Json.Decode.Pipeline exposing (..)
import Json.Encode as Encode
import Ports
import Random
import Random.List exposing (shuffle)
import RemoteData exposing (RemoteData)
import Task
import Time
import Url.Builder
import View



-- MODEL


type alias Meaning1 =
    Activity Trial State


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


type alias Model superModel =
    { superModel
        | meaning1 : Activity Trial State
        , optionsOrder : List Int
        , user : Maybe String
    }


infoLoaded : List ActivityInfo -> Meaning1 -> Meaning1
infoLoaded infos =
    Activity.infoLoaded
        Session1
        "Meaning 1"
        infos
        initialState


initialState : State
initialState =
    State "DefaultTrialUID" ""


init : String -> Model a -> ( Model a, Cmd Msg )
init group model =
    ( { model | meaning1 = Activity.loading }
    , Cmd.batch
        [ getRecords group
        , Ports.enableAlertOnExit ()
        ]
    )



-- VIEW


view : Model a -> Html Msg
view model =
    case model.meaning1 of
        Activity.Loading _ _ ->
            View.loading

        Activity.Err reason ->
            div [] [ text reason ]

        Activity.Running Activity.Training data ->
            case data.current of
                Just trial ->
                    div [ class "flex flex-col items-center" ]
                        [ p [] [ View.trainingWheelsGeneric (List.length data.history) data.infos.trainingWheel [ View.bold trial.writtenWord, View.bold trial.target ] ]
                        , p [] [ viewQuestion ("to " ++ trial.writtenWord) ]
                        , div
                            [ class "w-full ", disabled data.feedback ]
                          <|
                            View.shuffledOptions
                                data.state
                                data.feedback
                                UserClickedRadioButton
                                trial
                                model.optionsOrder
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

        Activity.Running Activity.Main data ->
            case data.current of
                Just trial ->
                    div [ class "flex flex-col items-center " ]
                        [ viewQuestion ("to " ++ trial.writtenWord)
                        , div
                            [ class " center-items justify-center w-full mt-6 ", disabled data.feedback ]
                          <|
                            View.shuffledOptions
                                data.state
                                data.feedback
                                UserClickedRadioButton
                                trial
                                model.optionsOrder
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
                    View.end data.infos.end SaveDataMsg (Just "spelling")

        Activity.NotStarted ->
            div [] [ text "I did not start yet." ]

        Activity.Running Activity.Instructions data ->
            View.instructions data.infos UserClickedStartTraining


viewQuestion : String -> Html msg
viewQuestion word =
    div [ class "text-3xl font-bold italic my-6" ] [ text word ]



-- UPDATE


type Msg
    = GotTrials (RemoteData Http.Error (List Trial))
    | GotRandomizedTrials (List Trial)
    | UserClickedNextTrial
    | NextTrial Time.Posix
    | UserClickedToggleFeedback
    | UserClickedRadioButton String
    | UserClickedStartMain
    | SaveDataMsg
    | UserClickedStartTraining
    | RuntimeShuffledOptionsOrder (List Int)
    | HistoryWasSaved (Result Http.Error String)


update : Msg -> Model superModel -> ( Model superModel, Cmd Msg )
update msg model =
    case msg of
        GotTrials (RemoteData.Success trials) ->
            ( model
            , Random.generate GotRandomizedTrials (shuffle trials)
            )

        GotRandomizedTrials trials ->
            ( { model | meaning1 = Activity.trialsLoaded trials initialState model.meaning1 }
            , Cmd.none
            )

        GotTrials (RemoteData.Failure error) ->
            ( { model
                | meaning1 = Activity.Err (Data.buildErrorMessage error)
              }
            , Cmd.none
            )

        GotTrials _ ->
            ( model, Cmd.none )

        UserClickedNextTrial ->
            ( model, Task.perform NextTrial Time.now )

        NextTrial timestamp ->
            let
                newModel =
                    { model | meaning1 = Activity.next timestamp initialState model.meaning1 }
            in
            ( newModel
            , Cmd.batch
                [ Random.generate RuntimeShuffledOptionsOrder (Random.List.shuffle model.optionsOrder)
                , saveData newModel
                ]
            )

        UserClickedToggleFeedback ->
            ( { model | meaning1 = Activity.toggle model.meaning1 }, Cmd.none )

        UserClickedRadioButton newChoice ->
            ( { model | meaning1 = Activity.update { uid = "", userAnswer = newChoice } model.meaning1 }, Cmd.none )

        UserClickedStartMain ->
            ( { model | meaning1 = Activity.startMain model.meaning1 initialState }, Cmd.none )

        -- data is now saved after each "trial", so this does nothing and shoud be removed
        SaveDataMsg ->
            ( model, Cmd.none )

        HistoryWasSaved _ ->
            ( model, Cmd.none )

        UserClickedStartTraining ->
            ( { model | meaning1 = Activity.startTraining model.meaning1 }, Cmd.none )

        RuntimeShuffledOptionsOrder newOrder ->
            ( { model | optionsOrder = newOrder }, Cmd.none )



-- HTTP


getRecords : String -> Cmd Msg
getRecords group =
    Http.get
        { url =
            Url.Builder.absolute [ ".netlify", "functions", "api" ]
                [ Url.Builder.string "base" "input"
                , Url.Builder.string "view" "Meaning"
                , Url.Builder.string "filterByFormula" ("{Classe} = \"" ++ group ++ "\"")
                ]
        , expect = Http.expectJson (RemoteData.fromResult >> GotTrials) decodeMeaningInput
        }


decodeMeaningInput : Decoder (List Trial)
decodeMeaningInput =
    let
        decoder =
            Decode.succeed Trial
                |> required "UID" string
                |> required "Word_Text" string
                |> required "Definition" string
                |> required "Distractor_1_Meaning" string
                |> required "Distractor_2_Meaning" string
                |> required "Distractor_3_Meaning" string
                |> required "Feedback_Incorrect_Meaning" string
                |> required "Feedback_Correct_Meaning" string
                |> optional "isTraining" bool False
    in
    decodeRecords decoder


getTrialsFromServer : (Result Http.Error (List Trial) -> msg) -> Cmd msg
getTrialsFromServer callbackMsg =
    Data.getTrialsFromServer_ "input" "Meaning" callbackMsg decodeMeaningInput


saveData : Model superModel -> Cmd Msg
saveData model =
    let
        history =
            Activity.getHistory model.meaning1
                |> List.filter (\( trial, _, _ ) -> not trial.isTraining)

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


updateHistoryEncoder : String -> List ( Trial, State, Time.Posix ) -> Encode.Value
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


historyEncoder : String -> List ( Trial, State, Time.Posix ) -> Encode.Value
historyEncoder _ history =
    Encode.object
        -- airtable does not support JSON columns, so we save giant JSON strings
        [ ( "Meaning1", Encode.string (Encode.encode 0 (Encode.list historyItemEncoder history)) )
        ]


historyItemEncoder : ( Trial, State, Time.Posix ) -> Encode.Value
historyItemEncoder ( { uid, writtenWord }, { userAnswer }, timestamp ) =
    Encode.object
        [ ( "trialUid", Encode.string uid )
        , ( "writtenWord", Encode.string writtenWord )
        , ( "answer", Encode.string userAnswer )
        , ( "answeredAt", Encode.int (Time.posixToMillis timestamp) )
        ]
