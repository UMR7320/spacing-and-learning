module Session1.Context1 exposing (..)

import Activity exposing (Activity)
import ActivityInfo exposing (ActivityInfo, Session(..))
import Data
import Html.Styled exposing (Html, div, p, span, text)
import Html.Styled.Attributes exposing (class)
import Http
import Json.Decode as Decode exposing (Decoder, string)
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


type alias Trial =
    { uid : String
    , text : String
    , target : String
    , infinitiveWord : String -- the non-modified word, like dread instead of dreading
    , distractor1 : String
    , distractor2 : String
    , distractor3 : String
    , definition : String
    , isTraining : Bool
    }


type alias State =
    { uid : String
    , userAnswer : String
    }


type alias Context1 =
    Activity Trial State


type alias Model superModel =
    { superModel
        | context1 : Activity Trial State
        , user : Maybe String
        , optionsOrder : List Int
    }


initState : State
initState =
    State "DefaultUid" ""


defaultTrial : Trial
defaultTrial =
    Trial "defaultuid" "defaulttarger" "defaultWrittenWord" "defaultText" "distractor1" "distractor2" "distractor3" "definition" False


infoLoaded : List ActivityInfo -> Context1 -> Context1
infoLoaded infos =
    Activity.infoLoaded
        Session1
        "Context 1"
        infos
        initState


init : String -> Model a -> ( Model a, Cmd Msg )
init group model =
    ( { model | context1 = Activity.loading model.context1 }
    , Cmd.batch
        [ getRecords group
        , Ports.enableAlertOnExit ()
        ]
    )



-- VIEW


view : Model a -> Html Msg
view model =
    case model.context1 of
        Activity.NotStarted ->
            div [] [ text "experiment did not start yet" ]

        Activity.Err reason ->
            div [] [ text reason ]

        Activity.Running Activity.Training data ->
            case data.current of
                Just trial ->
                    let
                        ( pre, post ) =
                            case String.split "/" trial.text of
                                x :: y :: _ ->
                                    ( x, y )

                                [ x ] ->
                                    ( x, "defaultPost" )

                                [] ->
                                    ( "defautpre", "defaultpOst" )
                    in
                    div [ class "flex flex-col items-center" ]
                        [ paragraphWithInput pre data.state.userAnswer post
                        , div [ class "w-full" ] <| View.shuffledOptions data.state data.feedback UserClickedRadioButton trial model.optionsOrder
                        , div [ class "col-start-2 col-span-4" ] <|
                            [ View.genericSingleChoiceFeedback
                                { isVisible = data.feedback
                                , userAnswer = data.state.userAnswer
                                , target = trial.target
                                , feedback_Correct = ( data.infos.feedback_correct, [ View.bold trial.infinitiveWord, View.bold trial.definition ] )
                                , feedback_Incorrect = ( data.infos.feedback_incorrect, [ View.bold trial.infinitiveWord, View.bold trial.definition ] )
                                , button = View.navigationButton UserClickedToggleFeedback UserClickedNextTrial data.feedback data.state.userAnswer
                                }
                            ]
                        ]

                Nothing ->
                    View.introToMain (UserClickedStartMain data.mainTrials data.infos)

        Activity.Running Activity.Main data ->
            case data.current of
                Just trial ->
                    let
                        ( pre, post ) =
                            case String.split "/" trial.text of
                                x :: y :: _ ->
                                    ( x, y )

                                [ x ] ->
                                    ( x, "defaultPost" )

                                [] ->
                                    ( "defautpre", "defaultpOst" )
                    in
                    div [ class "flex flex-col w-full items-center justify-center " ]
                        [ paragraphWithInput pre data.state.userAnswer post
                        , div [ class "w-full" ] <| View.shuffledOptions data.state data.feedback UserClickedRadioButton trial model.optionsOrder
                        , View.genericSingleChoiceFeedback
                            { isVisible = data.feedback
                            , userAnswer = data.state.userAnswer
                            , target = trial.target
                            , feedback_Correct = ( data.infos.feedback_correct, [ View.bold trial.infinitiveWord, View.bold trial.definition ] )
                            , feedback_Incorrect = ( data.infos.feedback_incorrect, [ View.bold trial.infinitiveWord, View.bold trial.definition ] )
                            , button = View.navigationButton UserClickedToggleFeedback UserClickedNextTrial data.feedback data.state.userAnswer
                            }
                        ]

                Nothing ->
                    View.end data.infos.end UserClickedSaveData (Just "wordcloud")

        Activity.Loading _ _ ->
            View.loading

        Activity.Running Activity.Instructions data ->
            div [] [ View.instructions data.infos UserClickedStartTraining ]


paragraphWithInput : String -> String -> String -> Html msg
paragraphWithInput pre userAnswer post =
    p [ class "bg-gray-200 mb-8 rounded-lg p-4" ]
        [ text pre
        , span [ class "border-4 h-2 pl-12 pr-12 font-bold" ]
            [ text <|
                if userAnswer == "I don't know" then
                    "???"

                else
                    userAnswer
            ]
        , text post
        ]



-- UPDATE


type Msg
    = GotTrials (RemoteData Http.Error (List Trial))
    | GotRandomizedTrials (List Trial)
    | UserClickedNextTrial
    | NextTrial Time.Posix
    | UserClickedToggleFeedback
    | UserClickedRadioButton String
    | UserClickedStartMain (List Trial) ActivityInfo
    | UserClickedSaveData
    | UserClickedStartTraining
    | RuntimeShuffledOptionsOrder (List Int)
    | HistoryWasSaved (Result Http.Error String)


update : Msg -> Model a -> ( Model a, Cmd Msg )
update msg model =
    case msg of
        GotTrials (RemoteData.Success trials) ->
            ( model
            , Random.generate GotRandomizedTrials (shuffle trials)
            )

        GotRandomizedTrials trials ->
            ( { model | context1 = Activity.trialsLoaded trials initState model.context1 }
            , Cmd.none
            )

        GotTrials (RemoteData.Failure error) ->
            ( { model | context1 = Activity.Err (Data.buildErrorMessage error) }
            , Cmd.none
            )

        GotTrials _ ->
            ( model, Cmd.none )

        UserClickedNextTrial ->
            ( model, Task.perform NextTrial Time.now )

        NextTrial timestamp ->
            let
                newModel =
                    { model | context1 = Activity.next timestamp initState model.context1 }
            in
            ( newModel
            , Cmd.batch
                [ Random.generate RuntimeShuffledOptionsOrder (Random.List.shuffle model.optionsOrder)
                , saveData newModel
                ]
            )

        UserClickedToggleFeedback ->
            ( { model | context1 = Activity.toggle model.context1 }, Cmd.none )

        UserClickedRadioButton newChoice ->
            ( { model | context1 = Activity.update { uid = "", userAnswer = newChoice } model.context1 }, Cmd.none )

        UserClickedStartMain _ _ ->
            ( { model | context1 = Activity.startMain model.context1 initState }, Cmd.none )

        -- data is now saved after each "trial", so this does nothing and shoud be removed
        UserClickedSaveData ->
            ( model, Cmd.none )

        UserClickedStartTraining ->
            ( { model | context1 = Activity.startTraining model.context1 }, Cmd.none )

        RuntimeShuffledOptionsOrder newOrder ->
            ( { model | optionsOrder = newOrder }, Cmd.none )

        HistoryWasSaved _ ->
            ( model, Cmd.none )



-- HTTP


decodeTrials : Decoder (List Trial)
decodeTrials =
    let
        decoder =
            Decode.succeed Trial
                |> required "UID" string
                |> required "Text_To_Complete" string
                |> required "Target_CU1" string
                |> required "Word_Text" string
                |> required "Distractor_1_CU_Lvl1" string
                |> required "Distractor_2_CU_Lvl1" string
                |> required "Distractor_3_CU_Lvl1" string
                |> required "Definition" string
                |> optional "isTraining" Decode.bool False
    in
    Data.decodeRecords decoder


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


saveData : Model a -> Cmd Msg
saveData model =
    let
        history =
            Activity.getHistory model.context1
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
                , ( "fields", historyEncoder history )
                ]
        )
        [ ( userId, history ) ]


historyEncoder : List ( Trial, State, Time.Posix ) -> Encode.Value
historyEncoder history =
    Encode.object
        -- airtable does not support JSON columns, so we save giant JSON strings
        [ ( "CU1", Encode.string (Encode.encode 0 (Encode.list historyItemEncoder history)) )
        ]


historyItemEncoder : ( Trial, State, Time.Posix ) -> Encode.Value
historyItemEncoder ( { uid, infinitiveWord }, { userAnswer }, timestamp ) =
    Encode.object
        [ ( "trialUid", Encode.string uid )
        , ( "target", Encode.string infinitiveWord )
        , ( "answer", Encode.string userAnswer )
        , ( "answeredAt", Encode.int (Time.posixToMillis timestamp) )
        ]
