module Session2.Context2 exposing (..)

import Activity exposing (Activity)
import ActivityInfo exposing (ActivityInfo, Session(..))
import Data
import Html.Styled as Html exposing (Html, div, input, label, p, text)
import Html.Styled.Attributes exposing (class, classList, value)
import Html.Styled.Events exposing (onInput)
import Http
import Json.Decode as Decode exposing (Decoder, string)
import Json.Decode.Pipeline exposing (..)
import Json.Encode as Encode
import Ports
import Random
import Random.List exposing (shuffle)
import RemoteData exposing (RemoteData)
import Route exposing (Session2Activity(..))
import Task
import Time
import Url.Builder
import View



-- MODEL


type Step
    = Listening Ntimes
    | Answering


type alias Ntimes =
    Int


type alias Trial =
    { uid : String
    , target : String
    , writtenWord : String -- can be different from target (go <-> went)
    , audioSentence : Data.AudioFile
    , textToComplete : String
    , feedback : String
    , isTraining : Bool
    }


type ResponseType
    = Speech
    | Thought


type alias State =
    { userAnswer : String
    , remainingListens : Int
    }


type alias Context2 =
    Activity Trial State


type alias Model superModel =
    { superModel
        | context2 : Context2
        , user : Maybe String
    }


initState : State
initState =
    State "" 3


defaultTrial : Trial
defaultTrial =
    Trial
        "defaultTrialUuuid"
        "defaultTrialTarget"
        "defaultTrialWrittenWord"
        (Data.AudioFile "" "")
        "defaultTextToComplete"
        "defaultFeedback"
        False


infoLoaded : List ActivityInfo -> Context2 -> Context2
infoLoaded infos =
    Activity.infoLoaded
        Session2
        "Context 2"
        infos
        initState


init : String -> Model a -> ( Model a, Cmd Msg )
init group model =
    ( { model | context2 = Activity.loading model.context2 }
    , Cmd.batch
        [ getRecords group
        , Ports.enableAlertOnExit ()
        ]
    )



-- VIEW


view : Model a -> Html Msg
view model =
    case model.context2 of
        Activity.NotStarted ->
            View.loading

        Activity.Loading _ _ ->
            View.loading

        Activity.Err reason ->
            div [] [ text reason ]

        Activity.Running Activity.Instructions data ->
            div [] [ View.instructions data.infos UserClickedStartTraining ]

        Activity.Running Activity.Training data ->
            viewTrialOrEnd data (View.introToMain (UserClickedStartMain data.mainTrials data.infos))

        Activity.Running Activity.Main data ->
            viewTrialOrEnd data (View.end data.infos.end UserClickedSaveData (Just "wordcloud"))


viewTrialOrEnd : Activity.Data Trial State -> Html Msg -> Html Msg
viewTrialOrEnd data endView =
    case data.current of
        Just trial ->
            viewTrial data trial

        Nothing ->
            endView


viewTrial : Activity.Data Trial State -> Trial -> Html Msg
viewTrial { state, feedback } trial =
    div [ class "context2" ]
        [ div []
            [ div [ class "audio-button" ]
                [ if state.remainingListens == 3 then
                    View.audioButton UserClickedAudio trial.audioSentence.url "Listen"

                  else if state.remainingListens == 2 then
                    View.audioButton UserClickedAudio trial.audioSentence.url "Listen again?"

                  else if state.remainingListens == 1 then
                    View.audioButton UserClickedAudio trial.audioSentence.url "Listen for the last time?"

                  else
                    text ""
                ]
            , div [] [ fillInTheBlanks trial.textToComplete trial.target state.userAnswer ]
            , div
                [ class "context2-input" ]
                [ label [] [ text "Your answer" ]
                , input
                    [ onInput UserAnswerChanged
                    , value state.userAnswer
                    ]
                    []
                ]
            ]
        , View.genericSingleChoiceFeedback
            { isVisible = feedback
            , userAnswer = state.userAnswer
            , target = trial.target
            , feedback_Correct = ( trial.feedback, [] )
            , feedback_Incorrect = ( trial.feedback, [] )
            , button = View.navigationButton UserClickedToggleFeedback UserClickedNextTrial feedback state.userAnswer
            }
        ]


audioButton : Trial -> Int -> Html Msg
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


fillInTheBlanks : String -> String -> String -> Html Msg
fillInTheBlanks textWithBlank target answer =
    let
        ( pre, post ) =
            case String.split "/" textWithBlank of
                x :: y :: _ ->
                    ( x, y )

                [ x ] ->
                    ( x, "" )

                [] ->
                    ( "", "" )

        options =
            [ "", "a" ++ target, answer ] -- ++ "a" is to add some padding

        viewOption option =
            div
                [ classList
                    [ ( "visible", option == answer )
                    , ( "empty", String.isEmpty option )
                    ]
                ]
                [ text option ]
    in
    p
        [ class "bg-gray-200 rounded-sm py-4 px-6 fill-in-the-blanks" ]
        [ div [] [ text pre ]
        , div
            [ class "blanks"
            , classList
                [ ( "right-aligned"
                  , post
                        |> String.toList
                        |> List.head
                        |> Maybe.map ((/=) ' ')
                        |> Maybe.withDefault False
                  )
                ]
            ]
            ([ "", "???" ] ++ options |> List.map viewOption)
        , div [] [ text post ]
        ]



-- UPDATE


type Msg
    = GotTrials (RemoteData Http.Error (List Trial))
    | GotRandomizedTrials (List Trial)
    | UserClickedNextTrial
    | NextTrial Time.Posix
    | UserClickedToggleFeedback
    | UserAnswerChanged String
    | UserClickedStartMain (List Trial) ActivityInfo
    | UserClickedSaveData
    | UserClickedAudio String
    | UserClickedStartTraining
    | HistoryWasSaved (Result Http.Error String)


update : Msg -> Model a -> ( Model a, Cmd Msg )
update msg model =
    let
        prevState =
            Activity.getState model.context2 |> Maybe.withDefault initState
    in
    case msg of
        GotTrials (RemoteData.Success trials) ->
            ( model
            , Random.generate GotRandomizedTrials (shuffle trials)
            )

        GotRandomizedTrials trials ->
            ( { model | context2 = Activity.trialsLoaded trials initState model.context2 }
            , Cmd.none
            )

        GotTrials (RemoteData.Failure error) ->
            ( { model
                | context2 = Activity.Err (Data.buildErrorMessage error)
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
                    { model | context2 = Activity.next timestamp initState model.context2 }
            in
            ( newModel
            , saveData newModel
            )

        UserClickedToggleFeedback ->
            ( { model | context2 = Activity.toggle model.context2 }, Cmd.none )

        UserAnswerChanged newChoice ->
            ( { model | context2 = Activity.update { prevState | userAnswer = newChoice } model.context2 }, Cmd.none )

        UserClickedStartMain _ _ ->
            ( { model | context2 = Activity.startMain model.context2 initState }, Cmd.none )

        -- data is now saved after each "trial", so this does nothing and shoud be removed
        UserClickedSaveData ->
            ( model, Cmd.none )

        HistoryWasSaved _ ->
            ( model, Cmd.none )

        UserClickedAudio url ->
            ( { model | context2 = Activity.update { prevState | remainingListens = prevState.remainingListens - 1 } model.context2 }
            , if prevState.remainingListens /= 0 then
                Ports.playAudio url

              else
                Cmd.none
            )

        UserClickedStartTraining ->
            ( { model | context2 = Activity.startTraining model.context2 }, Cmd.none )


decrement : Step -> Step
decrement step =
    case step of
        Listening nTimes ->
            Listening (nTimes - 1)

        _ ->
            Answering



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
        , expect = Http.expectJson (RemoteData.fromResult >> GotTrials) decodeTrials
        }


decodeTrials : Decoder (List Trial)
decodeTrials =
    let
        decoder =
            Decode.succeed Trial
                |> required "UID" string
                |> required "CU_Lvl2_target" string
                |> required "Word_Text" string
                |> required "Audio_Understanding" Data.decodeAudioFiles
                |> required "Text_To_Complete_2" string
                |> required "Feedback_CU_Lvl2" string
                |> optional "isTraining" Decode.bool False
    in
    Data.decodeRecords decoder


saveData : Model a -> Cmd Msg
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
historyItemEncoder ( { uid, writtenWord, target }, { userAnswer }, timestamp ) =
    Encode.object
        [ ( "trialUid", Encode.string uid )
        , ( "writtenWord", Encode.string writtenWord )
        , ( "target", Encode.string target )
        , ( "answer", Encode.string userAnswer )
        , ( "answeredAt", Encode.int (Time.posixToMillis timestamp) )
        ]
