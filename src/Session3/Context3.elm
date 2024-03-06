module Session3.Context3 exposing (..)

import Activity exposing (Activity)
import ActivityInfo exposing (ActivityInfo, Session(..))
import Data
import Delay
import Html.Styled as Html exposing (Html, div, label, text)
import Html.Styled.Attributes exposing (class, classList)
import Http
import Json.Decode as Decode exposing (Decoder, int, string)
import Json.Decode.Pipeline exposing (..)
import Json.Encode as Encode
import Ports
import Random
import Random.List
import Route exposing (Session3Activity(..))
import Task
import Time
import View
import Url.Builder
import RemoteData
import RemoteData exposing (RemoteData)
import Random.List exposing (shuffle)



-- MODEL


type alias Trial =
    { uid : String
    , writtenWord : String
    , audioSentence : Data.AudioFile
    , context : String
    , speaker1Name : String
    , speaker1Emoji : String
    , speaker1Line : String
    , speaker2Name : String
    , speaker2Emoji : String
    , speaker2Line : String
    , speaker2Time : Int
    , amorce : String
    , feedback : String
    , isTraining : Bool
    }


type alias State =
    { uid : String
    , userAnswer : String
    , step : Step
    , showSpeaker1 : Bool
    , showSpeaker2 : Bool
    }


type Step
    = Listening Int


type alias Context3 =
    Activity Trial State


defaultTrial : Trial
defaultTrial =
    { uid = ""
    , writtenWord = "String"
    , audioSentence = Data.AudioFile "" ""
    , context = "String"
    , speaker1Name = "String"
    , speaker1Emoji = "String"
    , speaker1Line = "String"
    , speaker2Name = "String"
    , speaker2Emoji = "String"
    , speaker2Line = "String"
    , speaker2Time = 500
    , amorce = "String"
    , feedback = "String"
    , isTraining = False
    }


type alias Model superModel =
    { superModel
        | context3 : Context3
        , user : Maybe String
        , optionsOrder : List Int
    }


initState : State
initState =
    State "DefaultUid" "" (Listening 3) False False


infoLoaded : List ActivityInfo -> Context3 -> Context3
infoLoaded infos =
    Activity.infoLoaded
        Session3
        "Context 3"
        infos
        initState


init : String -> Model a -> ( Model a, Cmd Msg )
init group model =
    ( { model | context3 = Activity.loading model.context3 }
    , Cmd.batch
        [ getRecords group
        , Ports.enableAlertOnExit ()
        ]
    )



-- VIEW


view : Model a -> Html Msg
view model =
    case model.context3 of
        Activity.NotStarted ->
            div [] [ text "Activity is not started yet." ]

        Activity.Loading _ _ ->
            View.loading

        Activity.Err reason ->
            div [] [ text <| "I stumbled into an error : " ++ reason ]

        Activity.Running Activity.Instructions data ->
            div [] [ View.instructions data.infos UserClickedStartTraining ]

        Activity.Running Activity.Training { current, state, feedback } ->
            case ( current, state.step ) of
                ( Just trial, Listening nTimes ) ->
                    let
                        ( context, _ ) =
                            case String.split "/" trial.context of
                                x :: xs ->
                                    ( x, String.concat xs )

                                _ ->
                                    ( "Missing context", "Missing dialog" )
                    in
                    viewStep context nTimes trial feedback state

                ( Nothing, _ ) ->
                    View.introToMain UserClickedStartMain

        Activity.Running Activity.Main ({ current, state, feedback } as data) ->
            case ( current, state.step ) of
                ( Just trial, Listening nTimes ) ->
                    let
                        ( context, _ ) =
                            case String.split "/" trial.context of
                                x :: xs ->
                                    ( x, String.concat xs )

                                _ ->
                                    ( "Missing context", "Missing dialog" )
                    in
                    viewStep context nTimes trial feedback state

                ( Nothing, _ ) ->
                    View.end data.infos.end UserClickedSaveData (Just "../post-tests/cw?session=S3")


viewStep : String -> Int -> Trial -> Bool -> State -> Html Msg
viewStep context nTimes trial feedback state =
    div [ class "flex flex-col items-center context-understanding-3 flow" ]
        [ div [ class "first-row" ]
            [ div
                [ class "p-4 bg-gray-200 rounded-lg context" ]
                [ View.fromMarkdown context ]
            , div [] [ viewLimitedTimesAudioButton nTimes trial ]
            ]
        , div
            []
            [ div [ class "context-understanding-3--grid" ]
                [ div
                    [ classList
                        [ ( "with-thought-bubble", True )
                        , ( "speaking", state.showSpeaker1 )
                        ]
                    ]
                    [ div [ class "avatar-with-name" ]
                        [ div [ class "text-4xl" ] [ text trial.speaker1Emoji ]
                        , text (trial.speaker1Name ++ " ")
                        ]
                    , div [ class "speech-bubble" ] [ text trial.speaker1Line ]
                    ]
                , div
                    [ classList
                        [ ( "with-thought-bubble bubble-left", True )
                        , ( "speaking", state.showSpeaker2 )
                        ]
                    ]
                    [ div [ class "avatar-with-name" ]
                        [ div [ class "text-4xl" ] [ text trial.speaker2Emoji ]
                        , text (trial.speaker2Name ++ " ")
                        ]
                    , div [ class "speech-bubble" ] [ text trial.speaker2Line ]
                    ]
                ]
            ]
        , label [] [ text "Your answer" ]
        , View.textAreaWithReadonlyAmorce
            { id_ = "production"
            , amorce = trial.amorce
            , isFeedback = feedback
            , userAnswer = state.userAnswer
            , onInputMsg = UserChangedInput
            }
        , View.genericNeutralFeedback
            { isVisible = feedback
            , feedback_Correct = ( trial.feedback, [] )
            , button = View.navigationButton UserClickedToggleFeedback UserClickedNextTrial feedback state.userAnswer
            }
        ]


viewLimitedTimesAudioButton : number -> Trial -> Html Msg
viewLimitedTimesAudioButton nTimes { audioSentence, speaker2Time } =
    if nTimes == 3 then
        View.audioButton (UserClickedAudio speaker2Time) audioSentence.url "Listen"

    else if nTimes == 2 then
        View.audioButton (UserClickedAudio speaker2Time) audioSentence.url "Listen again"

    else if nTimes == 1 then
        View.audioButton (UserClickedAudio speaker2Time) audioSentence.url "Listen one last time"

    else
        View.button { isDisabled = nTimes == 0, message = UserClickedStartAnswering, txt = "What happened?" }



-- UPDATE


type Msg
    = GotTrials (RemoteData Http.Error (List Trial))
    | GotRandomizedTrials (List Trial)
    | UserClickedNextTrial
    | NextTrial Time.Posix
    | UserClickedToggleFeedback
    | UserClickedStartMain
    | UserChangedInput String
    | UserClickedSaveData
    | UserClickedStartTraining
    | RuntimeShuffledOptionsOrder (List Int)
    | UserClickedAudio Int String
    | UserClickedStartAnswering
    | ShowSpeaker2
    | HistoryWasSaved (Result Http.Error String)


update : Msg -> Model a -> ( Model a, Cmd Msg )
update msg model =
    let
        prevState =
            Activity.getState model.context3 |> Maybe.withDefault initState
    in
    case msg of
        GotTrials (RemoteData.Success trials) ->
            ( model
            , Random.generate GotRandomizedTrials (shuffle trials)
            )

        GotRandomizedTrials trials ->
            ( { model | context3 = Activity.trialsLoaded trials initState model.context3 }
            , Cmd.none
            )

        GotTrials (RemoteData.Failure error) ->
            ( { model | context3 = Activity.Err (Data.buildErrorMessage error) }
            , Cmd.none
            )

        GotTrials _ ->
            ( model, Cmd.none )

        UserClickedNextTrial ->
            ( model, Task.perform NextTrial Time.now )

        NextTrial timestamp ->
            let
                newModel =
                    { model | context3 = Activity.next timestamp initState model.context3 }
            in
            ( newModel
            , Cmd.batch
                [ Random.generate RuntimeShuffledOptionsOrder
                    (Random.List.shuffle model.optionsOrder)
                , saveData newModel
                ]
            )

        UserClickedToggleFeedback ->
            ( { model | context3 = Activity.toggle model.context3 }, Cmd.none )

        UserClickedStartMain ->
            ( { model | context3 = Activity.startMain model.context3 initState }, Cmd.none )

        UserChangedInput new ->
            ( { model | context3 = Activity.update { prevState | userAnswer = new } model.context3 }, Cmd.none )

        -- data is now saved after each "trial", so this does nothing and shoud be removed
        UserClickedSaveData ->
            ( model, Cmd.none )

        HistoryWasSaved _ ->
            ( model, Cmd.none )

        UserClickedStartTraining ->
            ( { model | context3 = Activity.startTraining model.context3 }, Cmd.none )

        RuntimeShuffledOptionsOrder ls ->
            ( { model | optionsOrder = ls }, Cmd.none )

        UserClickedAudio speaker2Time url ->
            ( { model
                | context3 =
                    Activity.update
                        { prevState
                            | step = decrement prevState.step
                            , showSpeaker1 = True
                            , showSpeaker2 = False
                        }
                        model.context3
              }
            , if prevState.step /= Listening 0 then
                Cmd.batch
                    [ Ports.playAudio url
                    , Delay.after (speaker2Time * 10) ShowSpeaker2
                    ]

              else
                Delay.after 0 UserClickedStartAnswering
            )

        ShowSpeaker2 ->
            ( { model
                | context3 =
                    Activity.update
                        { prevState | showSpeaker1 = False, showSpeaker2 = True }
                        model.context3
              }
            , Cmd.none
            )

        UserClickedStartAnswering ->
            ( model, Cmd.none )


decrement : Step -> Step
decrement step =
    case step of
        Listening nTimes ->
            Listening (nTimes - 1)



-- HTTP

getRecords : String -> Cmd Msg
getRecords group =
    Http.get
        { url =
            Url.Builder.absolute [ ".netlify", "functions", "api" ]
                [ Url.Builder.string "base" "input"
                , Url.Builder.string "view" "ContextUnderstandingLvl1"
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
                |> required "Word_Text" string
                |> optional "CU_Lv3_Audio" Data.decodeAudioFiles (Data.AudioFile "" "")
                |> required "CU_Lvl3_Presentation" string
                |> required "CU_Lvl3_Speaker1Name" string
                |> required "CU_Lvl3_Speaker1Emoji" string
                |> required "CU_Lvl3_Speaker1Line" string
                |> required "CU_Lvl3_Speaker2Name" string
                |> required "CU_Lvl3_Speaker2Emoji" string
                |> required "CU_Lvl3_Speaker2Line" string
                |> required "CU_Lvl3_Speaker2_Time" int
                |> required "CU_Lvl3_TextToComplete_amorce" string
                |> required "CU_Lvl3_Feedback" string
                |> optional "isTraining" Decode.bool False
    in
    Data.decodeRecords decoder


saveData : Model a -> Cmd Msg
saveData model =
    let
        history =
            Activity.getHistory model.context3
                |> List.filter (\( trial, _, _ ) -> not trial.isTraining)

        userId =
            model.user |> Maybe.withDefault "recd18l2IBRQNI05y"

        payload =
            updateHistoryEncoder userId history
    in
    Http.request
        { method = "PATCH"
        , headers = []
        , url = Data.buildQuery { app = Data.apps.spacing, base = "users", view_ = "Session3_output" }
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
                , ( "fields", historyEncoder history )
                ]
        )
        [ ( userId, history ) ]


historyEncoder : List ( Trial, State, Time.Posix ) -> Encode.Value
historyEncoder history =
    Encode.object
        -- airtable does not support JSON columns, so we save giant JSON strings
        [ ( "CU3", Encode.string (Encode.encode 0 (Encode.list historyItemEncoder history)) )
        ]


historyItemEncoder : ( Trial, State, Time.Posix ) -> Encode.Value
historyItemEncoder ( { uid, writtenWord }, { userAnswer }, timestamp ) =
    Encode.object
        [ ( "trialUid", Encode.string uid )
        , ( "target", Encode.string writtenWord )
        , ( "answer", Encode.string userAnswer )
        , ( "answeredAt", Encode.int (Time.posixToMillis timestamp) )
        ]
