module Pretest.VKS exposing (..)

import Data
import Dict
import ExperimentInfo
import Html exposing (textarea)
import Html.Styled exposing (..)
import Html.Styled.Attributes as A
import Html.Styled.Events as E
import Http
import Json.Decode as Decode exposing (..)
import Json.Decode.Pipeline exposing (custom, optional, required)
import Json.Encode as Encode
import Logic
import Pretest.Acceptability exposing (ErrorBlock(..))
import Pretest.SPR exposing (Msg(..))
import Random
import Task
import View


taskId =
    "reczQs5ZD6g1x5F29"


type alias SC =
    Logic.Task Trial State


view : SC -> List (Html Msg)
view task =
    case task of
        Logic.Running Logic.Training data ->
            case data.current of
                Just trial ->
                    [ text "vks"
                    ]

                Nothing ->
                    [ div [ A.class "flex flex-col items-center" ]
                        [ View.fromMarkdown data.infos.introToMain
                        , View.button
                            { message = UserClickedStartMain data.infos data.mainTrials
                            , txt = "Start"
                            , isDisabled = False
                            }
                        ]
                    ]

        Logic.Running Logic.Main data ->
            case data.current of
                Just trial ->
                    [ p [ A.class "text-center text-lg border-2 m-2 p-2" ] [ text trial.context ]
                    , div [ A.class "flex flex-col items-center" ]
                        [ div [ A.class "order-first" ]
                            []
                        , div
                            [ A.class <|
                                "flex flex-col "
                                    ++ (if data.state.order == FirstProduction then
                                            "order-2"

                                        else
                                            "order-3"
                                       )
                            ]
                            [ h3 [] [ text "Complete this first text: " ]
                            , label [ A.for "firstProd" ] [ text trial.firstAmorce ]
                            , Html.Styled.textarea
                                [ A.id "firstProd"
                                , A.class "border-2"
                                , A.value data.state.firstProduction
                                , E.onInput (UserUpdatedField FirstProduction)
                                , A.spellcheck False
                                ]
                                []
                            ]
                        , div
                            [ A.class <|
                                "flex flex-col "
                                    ++ (if data.state.order == SecondProduction then
                                            "order-2"

                                        else
                                            "order-3"
                                       )
                            ]
                            [ text trial.secondAmorce
                            , Html.Styled.textarea
                                [ A.id "secondProd"
                                , A.class "border-2"
                                , A.value data.state.secondProduction
                                , E.onInput (UserUpdatedField SecondProduction)
                                , A.spellcheck False
                                ]
                                []
                            ]
                        , div [ A.class "order-last" ]
                            [ View.button
                                { message = UserClickedNextTrial
                                , txt = "Next Item"
                                , isDisabled = False
                                }
                            ]
                        ]
                    ]

                Nothing ->
                    [ View.end data.infos.end UserClickedSaveData "" ]

        Logic.Err reason ->
            [ text reason ]

        Logic.Loading ->
            [ text "Loading... Please don't quit or data may be lost" ]

        Logic.NotStarted ->
            [ text "C'est tout bon!" ]

        Logic.Running Logic.Instructions data ->
            []


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


type alias Trial =
    { id : String
    , context : String
    , firstAmorce : String
    , secondAmorce : String
    , isTraining : Bool
    , firstFeedback : String
    , secondFeedback : String
    }


type alias State =
    { firstProduction : String
    , secondProduction : String
    , order : Field
    }


type Msg
    = NoOp
    | RuntimeShuffledTrials ( List Trial, List ExperimentInfo.Task )
    | UserClickedToggleFeedback
    | UserClickedNextTrial
    | UserClickedStartMain ExperimentInfo.Task (List Trial)
    | UserClickedSaveData
    | ServerRespondedWithLastRecords (Result.Result Http.Error (List ()))
    | UserUpdatedField Field String
    | RuntimeReordedAmorces Field


type Field
    = FirstProduction
    | SecondProduction


type alias Model superModel =
    { superModel | vks : Logic.Task Trial State, user : Maybe String }


update : Msg -> Model superModel -> ( Model superModel, Cmd Msg )
update msg model =
    let
        prevState =
            Logic.getState model.vks |> Maybe.withDefault initState
    in
    case msg of
        RuntimeShuffledTrials ( trials, infos ) ->
            ( { model | vks = init infos trials }, Cmd.none )

        RuntimeReordedAmorces field ->
            ( { model | vks = model.vks |> Logic.update { prevState | order = field } }, Cmd.none )

        UserClickedNextTrial ->
            ( { model | vks = model.vks |> Logic.toggle |> Logic.next initState }, Random.generate RuntimeReordedAmorces (Random.uniform FirstProduction [ SecondProduction ]) )

        UserClickedToggleFeedback ->
            ( { model | vks = Logic.toggle model.vks }, Cmd.none )

        UserClickedStartMain infos trials ->
            ( { model | vks = Logic.startMain model.vks initState }, Cmd.none )

        UserUpdatedField fieldId new ->
            case fieldId of
                FirstProduction ->
                    ( { model | vks = model.vks |> Logic.update { prevState | firstProduction = new } }, Cmd.none )

                SecondProduction ->
                    ( { model | vks = model.vks |> Logic.update { prevState | secondProduction = new } }, Cmd.none )

        UserClickedSaveData ->
            let
                responseHandler =
                    ServerRespondedWithLastRecords
            in
            ( { model | vks = Logic.Loading }, saveData responseHandler model.user model.vks )

        ServerRespondedWithLastRecords (Result.Ok _) ->
            ( { model | vks = Logic.NotStarted }, Cmd.none )

        ServerRespondedWithLastRecords (Result.Err reason) ->
            ( { model | vks = Logic.Err (Data.buildErrorMessage reason) }, Cmd.none )

        NoOp ->
            ( model, Cmd.none )


init infos trials =
    let
        info =
            ExperimentInfo.toDict infos |> Dict.get taskId |> Result.fromMaybe "I couldn't find Task infos"
    in
    Logic.startIntro info (List.filter (\trial -> trial.isTraining) trials) (List.filter (\trial -> not trial.isTraining) trials) initState


initState =
    { firstProduction = "", secondProduction = "", order = FirstProduction }


saveData responseHandler maybeUserId task =
    let
        history =
            Logic.getHistory task

        userId =
            maybeUserId |> Maybe.withDefault "recd18l2IBRQNI05y"

        summarizedTrialEncoder =
            Encode.list
                (\( { id }, { firstProduction, secondProduction } ) ->
                    Encode.object
                        [ ( "fields"
                          , Encode.object
                                [ ( "sentenceCompletionTrialId", Encode.list Encode.string [ id ] )
                                , ( "firstProduction", Encode.string firstProduction )
                                , ( "secondProduction", Encode.string secondProduction )
                                ]
                          )
                        ]
                )

        sendInBatch_ =
            Data.sendInBatch summarizedTrialEncoder taskId userId history
    in
    Task.attempt responseHandler sendInBatch_
