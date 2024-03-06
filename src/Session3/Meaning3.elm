module Session3.Meaning3 exposing (..)

import Activity exposing (Activity)
import ActivityInfo exposing (ActivityInfo, Session(..))
import Data exposing (decodeRecords)
import Html.Styled exposing (Html, div, h4, p, span, text)
import Html.Styled.Attributes exposing (class)
import Http
import Json.Decode as Decode
import Json.Decode.Pipeline exposing (..)
import Json.Encode as Encode
import Ports
import Random
import Random.List exposing (shuffle)
import RemoteData exposing (RemoteData)
import Route exposing (Session3Activity(..))
import Task
import Time
import Url.Builder
import View



-- MODEL


type alias Trial =
    { uid : String
    , target : String
    , pre : String
    , stimulus : String
    , post : String
    , isTraining : Bool
    , radical : String
    }


type alias State =
    { uid : String
    , userAnswer : String
    }


type alias Meaning3 =
    Activity Trial State


type alias Model superModel =
    { superModel
        | meaning3 : Meaning3
        , user : Maybe String
    }


initState : State
initState =
    State "DefaultUserUID" ""


defaultTrial : Trial
defaultTrial =
    { uid = "uidMISSING"
    , target = "targetMISSING"
    , pre = "preMissing"
    , stimulus = "stimulusMissing"
    , post = "postMissing"
    , isTraining = False
    , radical = "defaultRadical"
    }


infoLoaded : List ActivityInfo -> Meaning3 -> Meaning3
infoLoaded infos =
    Activity.infoLoaded
        Session3
        "Meaning 3"
        infos
        initState


init : String -> Model a -> ( Model a, Cmd Msg )
init group model =
    ( model
    , Cmd.batch
        [ getRecords group
        , Ports.enableAlertOnExit ()
        ]
    )



-- VIEW


trainingWheels : Int -> String -> String -> Html msg
trainingWheels trialn radical target =
    let
        helpSentence =
            div [ class "flex flex-col pt-4 italic text-xl " ]
                [ p []
                    [ text "The synonym of "
                    , span [ class "font-bold" ] [ text radical ]
                    , text " is "
                    , span [ class "font-bold" ] [ text target ]
                    ]
                , span [] [ text "Type it here and you're good to go!" ]
                ]
    in
    case trialn of
        0 ->
            helpSentence

        _ ->
            div [] []


viewActivity : Activity.Activity Trial State -> List (Html Msg)
viewActivity experiment =
    case experiment of
        Activity.Err reason ->
            [ h4 [] [ p [] [ text ("Failure" ++ reason) ] ]
            ]

        Activity.NotStarted ->
            [ text "I'm not started yet." ]

        Activity.Loading _ _ ->
            [ View.loading ]

        Activity.Running Activity.Instructions data ->
            [ View.instructions data.infos UserCLickedStartTraining ]

        Activity.Running Activity.Training task ->
            case task.current of
                Just x ->
                    [ div [ class "flow" ]
                        [ View.sentenceInSynonym x task.state UserChangedInput task.feedback
                        , View.genericNeutralFeedback
                            { isVisible = task.feedback
                            , feedback_Correct = ( task.infos.feedback_correct, [ x.radical, x.target ] )
                            , button = View.navigationButton UserClickedFeedback UserClickedNextTrial task.feedback task.state.userAnswer
                            }
                        ]
                    ]

                Nothing ->
                    [ View.introToMain <| UserClickedStartMainloop ]

        Activity.Running Activity.Main task ->
            case task.current of
                Just x ->
                    [ div [ class "flow" ]
                        [ View.sentenceInSynonym x task.state UserChangedInput task.feedback
                        , View.genericNeutralFeedback
                            { isVisible = task.feedback
                            , feedback_Correct = ( task.infos.feedback_correct, [ x.radical, x.target ] )
                            , button = View.navigationButton UserClickedFeedback UserClickedNextTrial task.feedback task.state.userAnswer
                            }
                        ]
                    ]

                Nothing ->
                    [ View.end task.infos.end SaveDataMsg (Just "spelling") ]



-- UPDATE


type Msg
    = GotTrials (RemoteData Http.Error (List Trial))
    | GotRandomizedTrials (List Trial)
    | UserClickedFeedback
    | UserChangedInput String
    | UserClickedNextTrial
    | NextTrial Time.Posix
    | UserClickedStartMainloop
    | SaveDataMsg
    | ServerRespondedWithLastRecords (Result Http.Error (List ()))
    | UserCLickedStartTraining
    | HistoryWasSaved (Result Http.Error String)


update : Msg -> Model a -> ( Model a, Cmd Msg )
update msg model =
    case msg of
        GotTrials (RemoteData.Success trials) ->
            ( model
            , Random.generate GotRandomizedTrials (shuffle trials)
            )

        GotRandomizedTrials trials ->
            ( { model | meaning3 = Activity.trialsLoaded trials initState model.meaning3 }
            , Cmd.none
            )

        GotTrials (RemoteData.Failure error) ->
            ( { model | meaning3 = Activity.Err (Data.buildErrorMessage error) }
            , Cmd.none
            )

        GotTrials _ ->
            ( model, Cmd.none )

        UserClickedFeedback ->
            ( { model
                | meaning3 =
                    model.meaning3
                        |> Activity.toggle
              }
            , Cmd.none
            )

        UserChangedInput newChoice ->
            ( { model
                | meaning3 =
                    model.meaning3
                        |> Activity.update { uid = "", userAnswer = newChoice }
              }
            , Cmd.none
            )

        UserClickedNextTrial ->
            ( model, Task.perform NextTrial Time.now )

        NextTrial timestamp ->
            let
                newModel =
                    { model
                        | meaning3 =
                            model.meaning3 |> Activity.next timestamp initState
                    }
            in
            ( newModel
            , saveData newModel
            )

        -- data is now saved after each "trial", so this does nothing and shoud be removed
        SaveDataMsg ->
            ( model, Cmd.none )

        HistoryWasSaved _ ->
            ( model, Cmd.none )

        UserClickedStartMainloop ->
            ( { model | meaning3 = Activity.startMain model.meaning3 initState }, Cmd.none )

        ServerRespondedWithLastRecords _ ->
            ( model, Cmd.none )

        UserCLickedStartTraining ->
            ( { model | meaning3 = Activity.startTraining model.meaning3 }, Cmd.none )



-- HTTP


decodeTrials : Decode.Decoder (List Trial)
decodeTrials =
    let
        decoder =
            Decode.succeed Trial
                |> required "UID" Decode.string
                |> required "Word_Text" Decode.string
                |> required "pre" Decode.string
                |> required "stim" Decode.string
                |> required "post" Decode.string
                |> optional "isTraining" Decode.bool False
                |> required "radical" Decode.string
    in
    decodeRecords decoder


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


saveData : Model a -> Cmd Msg
saveData model =
    let
        history =
            Activity.getHistory model.meaning3
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
        [ ( "Meaning3", Encode.string (Encode.encode 0 (Encode.list historyItemEncoder history)) )
        ]


historyItemEncoder : ( Trial, State, Time.Posix ) -> Encode.Value
historyItemEncoder ( { uid, target }, { userAnswer }, timestamp ) =
    Encode.object
        [ ( "trialUid", Encode.string uid )
        , ( "target", Encode.string target )
        , ( "answer", Encode.string userAnswer )
        , ( "answeredAt", Encode.int (Time.posixToMillis timestamp) )
        ]
