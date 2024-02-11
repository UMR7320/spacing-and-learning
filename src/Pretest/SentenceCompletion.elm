module Pretest.SentenceCompletion exposing (..)

import Data
import ActivityInfo exposing (ActivityInfo)
import Html.Styled exposing (..)
import Html.Styled.Attributes as A
import Html.Styled.Events as E
import Http
import Json.Decode as Decode
import Json.Decode.Pipeline exposing (optional, required)
import Json.Encode as Encode
import Activity exposing (Activity)
import Pretest.Version exposing (Version(..))
import Random
import Task
import Time
import View



-- MODEL


type alias Trial =
    { id : String
    , context : String
    , firstAmorce : String
    , secondAmorce : String
    , isTraining : Bool
    , firstFeedback : String
    , secondFeedback : String
    }


type Field
    = FirstProduction
    | SecondProduction


type alias State =
    { firstProduction : String
    , secondProduction : String
    , order : Field
    }


type alias SentenceCompletion =
    Activity Trial State


type alias Model superModel =
    { superModel
        | sentenceCompletion : Activity Trial State
        , user : Maybe String
        , version : Version
    }


init infos trials version =
    let
        info =
           infos
           |> List.filter (\task -> task.session == Pretest.Version.toSession version && task.name == "Writing test")
           |> List.head
           |> Result.fromMaybe ("Could not find SPR info for version " ++ Pretest.Version.toString version)
    in
    Activity.startIntro info (List.filter (\trial -> trial.isTraining) trials) (List.filter (\trial -> not trial.isTraining) trials) initState


initState =
    { firstProduction = "", secondProduction = "", order = FirstProduction }



-- VIEW


view : SentenceCompletion -> List (Html Msg)
view task =
    case task of
        Activity.Running Activity.Training data ->
            case data.current of
                Just trial ->
                    [ div [ A.class "flex flex-col items-center" ]
                        [ p [ A.class "w-full max-w-2xl  m-4 p-2" ] [ text trial.context ]
                        , Html.Styled.textarea
                            [ A.id "firstProd"
                            , A.class "border-2 w-full max-w-2xl  rounded-lg p-2"
                            , A.value <|
                                readOnlyAmorce
                                    trial.firstAmorce
                                    data.state.firstProduction
                            , E.onInput (UserUpdatedField FirstProduction)
                            , A.spellcheck False
                            , A.placeholder trial.firstAmorce
                            , readOnlyOnFeedback data
                            ]
                            [ text trial.firstAmorce ]
                        , if data.feedback then
                            div [ A.class "pb-8 pt-2 w-full max-w-2xl p-2 bg-gray-200 rounded-lg m-4" ] [ View.fromMarkdown trial.firstFeedback ]

                          else
                            text ""
                        , Html.Styled.textarea
                            [ A.id "secondProd"
                            , A.class "border-2 w-full max-w-2xl rounded-lg p-2"
                            , E.onInput (UserUpdatedField SecondProduction)
                            , A.spellcheck False
                            , readOnlyOnFeedback data
                            , A.value <|
                                readOnlyAmorce
                                    trial.secondAmorce
                                    data.state.secondProduction
                            ]
                            [ text trial.secondAmorce ]
                        , if data.feedback then
                            div [ A.class "pt-2 w-full max-w-2xl bg-gray-200 p-2 m-4 rounded-lg" ] [ View.fromMarkdown trial.secondFeedback ]

                          else
                            text ""
                        , if data.feedback then
                            div [ A.class "items-center font-bold text-green-500" ] [ text "These are of course only suggestions, there are many other possibilities!" ]

                          else
                            div [] []
                        , View.navigationButton UserClickedToggleFeedback UserClickedNextTrial data.feedback data.state.firstProduction
                        ]
                    ]

                Nothing ->
                    [ div [ A.class "flex flex-col items-center flow" ]
                        [ View.fromMarkdown data.infos.introToMain
                        , View.button
                            { message = UserClickedStartMain data.infos data.mainTrials
                            , txt = "Start"
                            , isDisabled = False
                            }
                        ]
                    ]

        Activity.Running Activity.Main data ->
            case data.current of
                Just trial ->
                    [ div [ A.class "flex flex-col w-full items-center" ]
                        [ p [ A.class "max-w-2xl m-4 p-2" ] [ text trial.context ]
                        , Html.Styled.textarea
                            [ A.id "firstProd"
                            , A.class "border-2 w-full max-w-2xl p-2"
                            , A.class <|
                                if data.state.order == FirstProduction then
                                    " order-2"

                                else
                                    " order-3"
                            , E.onInput (UserUpdatedField FirstProduction)
                            , A.spellcheck False
                            , A.value <|
                                readOnlyAmorce
                                    trial.firstAmorce
                                    data.state.firstProduction
                            ]
                            [ text trial.firstAmorce ]
                        , Html.Styled.textarea
                            [ A.id "secondProd"
                            , A.class "border-2 mt-4 w-full max-w-2xl p-2 w-1/3"
                            , E.onInput (UserUpdatedField SecondProduction)
                            , A.spellcheck False
                            , A.class <|
                                if data.state.order == SecondProduction then
                                    " order-2"

                                else
                                    " order-3"
                            , A.value <|
                                readOnlyAmorce
                                    trial.secondAmorce
                                    data.state.secondProduction
                            ]
                            [ text trial.secondAmorce ]
                        , div [ A.class "order-last" ]
                            [ View.button
                                { message = UserClickedNextTrial
                                , txt = "Continue"
                                , isDisabled = False
                                }
                            ]
                        ]
                    ]

                Nothing ->
                    [ View.end data.infos.end UserClickedSaveData "acceptability/instructions" ]

        Activity.Err reason ->
            [ text reason ]

        Activity.Loading ->
            [ View.loading ]

        Activity.NotStarted ->
            [ text "C'est tout bon!" ]

        Activity.Running Activity.Instructions data ->
            [ View.instructions data.infos UserClickedStartTraining ]


readOnlyOnFeedback data =
    if data.feedback then
        A.readonly True

    else
        A.readonly False


readOnlyAmorce firstAmorce firstProduction =
    if String.length firstProduction < String.length firstAmorce then
        firstAmorce

    else
        firstProduction



-- UPDATE


type Msg
    = UserClickedToggleFeedback
    | UserClickedNextTrial
    | NextTrial Time.Posix
    | UserClickedStartMain ActivityInfo (List Trial)
    | UserClickedSaveData
    | UserUpdatedField Field String
    | RuntimeReordedAmorces Field
    | UserClickedStartTraining
    | HistoryWasSaved (Result Http.Error String)


update : Msg -> Model superModel -> ( Model superModel, Cmd Msg )
update msg model =
    let
        prevState =
            Activity.getState model.sentenceCompletion |> Maybe.withDefault initState
    in
    case msg of
        RuntimeReordedAmorces field ->
            ( { model | sentenceCompletion = model.sentenceCompletion |> Activity.update { prevState | order = field } }, Cmd.none )

        UserClickedNextTrial ->
            ( model, Task.perform NextTrial Time.now )

        NextTrial timestamp ->
            ( { model
                | sentenceCompletion =
                    model.sentenceCompletion
                        |> Activity.toggle
                        |> Activity.next timestamp initState
              }
            , Cmd.batch
                [ Random.generate
                    RuntimeReordedAmorces
                    (Random.uniform FirstProduction [ SecondProduction ])
                , saveData model
                ]
            )

        UserClickedToggleFeedback ->
            ( { model | sentenceCompletion = Activity.toggle model.sentenceCompletion }, Cmd.none )

        UserClickedStartMain _ _ ->
            ( { model | sentenceCompletion = Activity.startMain model.sentenceCompletion initState }, Cmd.none )

        UserUpdatedField fieldId new ->
            case fieldId of
                FirstProduction ->
                    ( { model | sentenceCompletion = model.sentenceCompletion |> Activity.update { prevState | firstProduction = new } }, Cmd.none )

                SecondProduction ->
                    ( { model | sentenceCompletion = model.sentenceCompletion |> Activity.update { prevState | secondProduction = new } }, Cmd.none )

        UserClickedSaveData ->
            ( { model | sentenceCompletion = Activity.Loading }, Cmd.none )

        UserClickedStartTraining ->
            ( { model | sentenceCompletion = Activity.startTraining model.sentenceCompletion }, Cmd.none )

        HistoryWasSaved _ ->
            ( model, Cmd.none )



-- HTTP


getRecords =
    Http.task
        { method = "GET"
        , headers = []
        , url =
            Data.buildQuery
                { app = Data.apps.spacing
                , base = "sentence_completion"
                , view_ = "all"
                }
        , body = Http.emptyBody
        , resolver = Http.stringResolver <| Data.handleJsonResponse <| decodeAcceptabilityTrials
        , timeout = Just 5000
        }


decodeAcceptabilityTrials : Decode.Decoder (List Trial)
decodeAcceptabilityTrials =
    let
        decoder =
            Decode.succeed Trial
                |> required "id" Decode.string
                |> required "context" Decode.string
                |> required "amorce1" Decode.string
                |> required "amorce2" Decode.string
                |> optional "isTraining" Decode.bool False
                |> optional "feedback1" Decode.string "Missing feedback"
                |> optional "feedback2" Decode.string "Missing feedback 2"
    in
    Data.decodeRecords decoder


saveData model =
    let
        history =
            Activity.getHistory model.sentenceCompletion

        userId =
            model.user |> Maybe.withDefault "recd18l2IBRQNI05y"

        payload =
            updateHistoryEncoder model.version userId history
    in
    Http.request
        { method = "PATCH"
        , headers = []
        , url = Data.buildQuery { app = Data.apps.spacing, base = "users", view_ = "SPR_output" }
        , body = Http.jsonBody payload
        , expect = Http.expectJson HistoryWasSaved (Decode.succeed "OK")
        , timeout = Nothing
        , tracker = Nothing
        }


updateHistoryEncoder : Version -> String -> List ( Trial, State, Time.Posix ) -> Encode.Value
updateHistoryEncoder version userId history =
    -- The Netflify function that receives PATCH requests only works with arrays
    Encode.list
        (\_ ->
            Encode.object
                [ ( "id", Encode.string userId )
                , ( "fields", historyEncoder version userId history )
                ]
        )
        [ ( version, userId, history ) ]


historyEncoder : Version -> String -> List ( Trial, State, Time.Posix ) -> Encode.Value
historyEncoder version userId history =
    let
        answerField =
            case version of
                PreTest ->
                    "SentenceCompletion_preTest"

                PostTest ->
                    "SentenceCompletion_postTest"

                PostTestDiff ->
                    "SentenceCompletion_postTestDiff"

                Surprise ->
                    "SentenceCompletion_surprisePostTest"

                Unknown val ->
                    "SentenceCompletion_" ++ val
    in
    Encode.object
        -- airtable does not support JSON columns, so we save giant JSON strings
        [ ( answerField, Encode.string (Encode.encode 0 (Encode.list historyItemEncoder history)) )
        ]


historyItemEncoder : ( Trial, State, Time.Posix ) -> Encode.Value
historyItemEncoder ( { id, firstAmorce, secondAmorce }, { firstProduction, secondProduction }, timestamp ) =
    Encode.object
        [ ( "trialId", Encode.string id )
        , ( "firstAmorce", Encode.string firstAmorce )
        , ( "secondAmorce", Encode.string secondAmorce )
        , ( "firstProduction", Encode.string firstProduction )
        , ( "secondProduction", Encode.string secondProduction )
        , ( "answeredAt", Encode.int (Time.posixToMillis timestamp) )
        ]
