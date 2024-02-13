module Session2.Context2 exposing (..)

import Activity exposing (Activity)
import ActivityInfo exposing (ActivityInfo, Session(..))
import Data
import Delay
import Html.Styled as Html exposing (div, text)
import Html.Styled.Attributes exposing (class)
import Http exposing (Error)
import Json.Decode as Decode exposing (Decoder, string)
import Json.Decode.Pipeline exposing (..)
import Json.Encode as Encode
import Ports
import Random
import Random.List
import Task
import Time
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


type alias Context2 =
    Activity Trial State


initState : State
initState =
    State "" (Listening 3)


defaultTrial : Trial
defaultTrial =
    Trial "defaultTrial" "defaultTrial" (Data.AudioFile "" "") "defautcontext" "defaulttarget" "defautdis1" "defaultdis2" "defaultdis3" "defaultfeedback" "defaultName" Speech False


start : List ActivityInfo -> List Trial -> Activity Trial State
start info trials =
    Activity.startIntro
        (ActivityInfo.activityInfo info Session2 "Context 2")
        (List.filter (\datum -> datum.isTraining) trials)
        (List.filter (\datum -> not datum.isTraining) trials)
        initState


infoLoaded : List ActivityInfo -> Context2 -> Context2
infoLoaded infos =
    Activity.infoLoaded
        Session2
        "Context 2"
        infos
        initState



-- VIEW


view exp optionsOrder =
    case exp of
        Activity.NotStarted ->
            div [] [ text "experiment did not start yet" ]

        Activity.Loading _ _ ->
            View.loading

        Activity.Err reason ->
            div [] [ text reason ]

        Activity.Running Activity.Instructions data ->
            div [] [ View.instructions data.infos UserClickedStartTraining ]

        Activity.Running Activity.Training data ->
            viewTrialOrEnd optionsOrder data (View.introToMain (UserClickedStartMain data.mainTrials data.infos))

        Activity.Running Activity.Main data ->
            viewTrialOrEnd optionsOrder data (View.end data.infos.end UserClickedSaveData (Just "../post-tests/cw?session=S2"))


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
                                div [ class "text-4xl" ] [ text "🤔" ]
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


type Msg
    = UserClickedNextTrial
    | NextTrial Time.Posix
    | UserClickedToggleFeedback
    | UserClickedRadioButton String
    | UserClickedStartMain (List Trial) ActivityInfo
    | UserClickedSaveData
    | UserClickedAudio String
    | RuntimeShuffledOptionsOrder (List Int)
    | UserClickedStartTraining
    | UserClickedStartAnswering
    | HistoryWasSaved (Result Http.Error String)


update msg model =
    let
        prevState =
            Activity.getState model.context2 |> Maybe.withDefault initState
    in
    case msg of
        UserClickedNextTrial ->
            ( model, Task.perform NextTrial Time.now )

        NextTrial timestamp ->
            let
                newModel =
                    { model | context2 = Activity.next timestamp initState model.context2 }
            in
            ( newModel
            , Cmd.batch
                [ Random.generate RuntimeShuffledOptionsOrder (Random.List.shuffle model.optionsOrder)
                , saveData newModel
                ]
            )

        UserClickedToggleFeedback ->
            ( { model | context2 = Activity.toggle model.context2 }, Cmd.none )

        UserClickedRadioButton newChoice ->
            ( { model | context2 = Activity.update { prevState | userAnswer = newChoice } model.context2 }, Cmd.none )

        UserClickedStartMain _ _ ->
            ( { model | context2 = Activity.startMain model.context2 initState }, Cmd.none )

        -- data is now saved after each "trial", so this does nothing and shoud be removed
        UserClickedSaveData ->
            ( model, Cmd.none )

        HistoryWasSaved _ ->
            ( model, Cmd.none )

        UserClickedAudio url ->
            ( { model | context2 = Activity.update { prevState | step = decrement prevState.step } model.context2 }
            , if prevState.step /= Listening 0 then
                Ports.playAudio url

              else
                Delay.after 0 UserClickedStartAnswering
            )

        RuntimeShuffledOptionsOrder ls ->
            ( { model | optionsOrder = ls }, Cmd.none )

        UserClickedStartTraining ->
            ( { model | context2 = Activity.startTraining model.context2 }, Cmd.none )

        UserClickedStartAnswering ->
            ( { model | context2 = Activity.update { prevState | step = Answering } model.context2 }, Cmd.none )


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


saveData model =
    let
        history =
            Activity.getHistory model.context2
                |> List.filter (\( trial, _, _ ) -> not trial.isTraining)

        userId =
            model.user |> Maybe.withDefault "recd18l2IBRQNI05y"

        payload =
            updateHistoryEncoder userId history
    in
    Http.request
        { method = "PATCH"
        , headers = []
        , url = Data.buildQuery { app = Data.apps.spacing, base = "users", view_ = "Session2_output" }
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
        [ ( "CU2", Encode.string (Encode.encode 0 (Encode.list historyItemEncoder history)) )
        ]


historyItemEncoder : ( Trial, State, Time.Posix ) -> Encode.Value
historyItemEncoder ( { uid, writtenWord }, { userAnswer }, timestamp ) =
    Encode.object
        [ ( "trialUid", Encode.string uid )
        , ( "writtenWord", Encode.string writtenWord )
        , ( "answer", Encode.string userAnswer )
        , ( "answeredAt", Encode.int (Time.posixToMillis timestamp) )
        ]



-- INTERNALS


taskId =
    "recwxsmowpB18bpLj"
