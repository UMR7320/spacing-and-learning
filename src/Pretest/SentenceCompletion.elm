module Pretest.SentenceCompletion exposing (..)

import Data
import Dict
import ExperimentInfo
import Html.Styled exposing (..)
import Html.Styled.Attributes as A
import Html.Styled.Events as E
import Http
import Json.Decode as Decode
import Json.Decode.Pipeline exposing (optional, required)
import Json.Encode as Encode
import Logic
import Pretest.Acceptability exposing (ErrorBlock(..))
import Pretest.SPR exposing (Msg(..))
import Progressbar exposing (progressBar)
import Random
import Task
import View


taskId =
    "reczQs5ZD6g1x5F29"


type alias SentenceCompletion =
    Logic.Task Trial State


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


view : SentenceCompletion -> List (Html Msg)
view task =
    case task of
        Logic.Running Logic.Training data ->
            case data.current of
                Just trial ->
                    [ div [ A.class "flex flex-col" ]
                        [ p [ A.class "text-lg  m-4 p-2" ] [ text trial.context ]
                        , Html.Styled.textarea
                            [ A.id "firstProd"
                            , A.class "border-2 rounded-lg p-2"
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
                            div [ A.class "pb-8 pt-2 p-2 bg-gray-200 rounded-lg m-4" ] [ View.fromMarkdown trial.firstFeedback ]

                          else
                            text ""
                        , Html.Styled.textarea
                            [ A.id "secondProd"
                            , A.class "border-2 rounded-lg p-2"
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
                            div [ A.class "pt-2 bg-gray-200 p-2 m-4 rounded-lg" ] [ View.fromMarkdown trial.secondFeedback ]

                          else
                            text ""
                        , if data.feedback then
                            div [ A.class "items-center @text-lg font-bold text-green-500" ] [ text "These are of course only suggestions, there are many other possibilities!" ]

                          else
                            div [] []
                        ]
                    , View.navigationButton UserClickedToggleFeedback UserClickedNextTrial data.feedback data.state.firstProduction
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
                    [ div [ A.class "flex flex-col w-full items-center" ]
                        [ progressBar data.history data.mainTrials
                        , p [ A.class "text-lg  m-4 p-2" ] [ text trial.context ]
                        , Html.Styled.textarea
                            [ A.id "firstProd"
                            , A.class "border-2 p-2 w-full"
                            , A.class <|
                                if data.state.order == FirstProduction then
                                    "order-2"

                                else
                                    "order-3"
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
                            , A.class "border-2 mt-4 p-2 w-full"
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
                    [ View.end data.infos.end UserClickedSaveData "" ]

        Logic.Err reason ->
            [ text reason ]

        Logic.Loading ->
            [ text "Loading... Please don't quit or data may be lost" ]

        Logic.NotStarted ->
            [ text "C'est tout bon!" ]

        Logic.Running Logic.Instructions data ->
            [ View.instructions data.infos.instructions UserClickedStartTraining ]


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
    = UserClickedToggleFeedback
    | UserClickedNextTrial
    | UserClickedStartMain ExperimentInfo.Task (List Trial)
    | UserClickedSaveData
    | ServerRespondedWithLastRecords (Result.Result Http.Error (List ()))
    | UserUpdatedField Field String
    | RuntimeReordedAmorces Field
    | UserClickedStartTraining


type Field
    = FirstProduction
    | SecondProduction


type alias Model superModel =
    { superModel | sentenceCompletion : Logic.Task Trial State, user : Maybe String }


update : Msg -> Model superModel -> ( Model superModel, Cmd Msg )
update msg model =
    let
        prevState =
            Logic.getState model.sentenceCompletion |> Maybe.withDefault initState
    in
    case msg of
        RuntimeReordedAmorces field ->
            ( { model | sentenceCompletion = model.sentenceCompletion |> Logic.update { prevState | order = field } }, Cmd.none )

        UserClickedNextTrial ->
            ( { model | sentenceCompletion = model.sentenceCompletion |> Logic.toggle |> Logic.next initState }, Random.generate RuntimeReordedAmorces (Random.uniform FirstProduction [ SecondProduction ]) )

        UserClickedToggleFeedback ->
            ( { model | sentenceCompletion = Logic.toggle model.sentenceCompletion }, Cmd.none )

        UserClickedStartMain _ _ ->
            ( { model | sentenceCompletion = Logic.startMain model.sentenceCompletion initState }, Cmd.none )

        UserUpdatedField fieldId new ->
            case fieldId of
                FirstProduction ->
                    ( { model | sentenceCompletion = model.sentenceCompletion |> Logic.update { prevState | firstProduction = new } }, Cmd.none )

                SecondProduction ->
                    ( { model | sentenceCompletion = model.sentenceCompletion |> Logic.update { prevState | secondProduction = new } }, Cmd.none )

        UserClickedSaveData ->
            let
                responseHandler =
                    ServerRespondedWithLastRecords
            in
            ( { model | sentenceCompletion = Logic.Loading }, saveData responseHandler model.user model.sentenceCompletion )

        ServerRespondedWithLastRecords (Result.Ok _) ->
            ( { model | sentenceCompletion = Logic.NotStarted }, Cmd.none )

        ServerRespondedWithLastRecords (Result.Err reason) ->
            ( { model | sentenceCompletion = Logic.Err (Data.buildErrorMessage reason) }, Cmd.none )

        UserClickedStartTraining ->
            ( { model | sentenceCompletion = Logic.startTraining model.sentenceCompletion }, Cmd.none )


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
