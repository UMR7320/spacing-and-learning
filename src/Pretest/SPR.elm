module Pretest.SPR exposing (..)

import Browser.Events exposing (onKeyDown)
import Data exposing (decodeRecords)
import Dict
import ExperimentInfo
import Html.Styled exposing (..)
import Html.Styled.Attributes as Attr
import Http
import Json.Decode as Decode
import Json.Decode.Pipeline exposing (custom, optional, required)
import Json.Encode as Encode
import Logic
import Task
import Task.Parallel as Para
import Time
import View


taskId =
    "rec7oxQBDY7rBTRDn"


type alias Pretest =
    Para.State2 Msg (List Trial) (List ExperimentInfo.Task)


type alias SPR =
    Logic.Task Trial State


type alias Spr model =
    { model
        | spr : Logic.Task Trial State

        --, pretest : Para.State2 Msg (List Trial) (List ExperimentInfo.Task)
        , user : Maybe String
    }



-- Types


type alias Trial =
    { id : String
    , taggedSegments : List TaggedSegment
    , question : String
    , isGrammatical : Bool
    , isTraining : Bool
    , feedback : String
    }


type Msg
    = NoOp
    | ServerRespondedWithLastRecords (Result.Result Http.Error (List ()))
    | StartMain ExperimentInfo.Task (List Trial)
    | TimestampedMsg TimedMsg (Maybe Time.Posix)
    | UserClickedNextTrial Answer
    | UserClickedSaveData
    | UserConfirmedChoice Answer
    | UserClickedStartTraining


type TimedMsg
    = UserPressedSpaceToStartParagraph
    | UserPressedSpaceToReadNextSegment


type Tag
    = NoUnit
    | Critic
    | SpillOver


tagToString tag =
    case tag of
        NoUnit ->
            "No unit"

        Critic ->
            "Critic"

        SpillOver ->
            "SpillOver"


type alias TaggedSegment =
    ( Tag, String )


type Step
    = SPR SprStep
    | Feedback
    | Question


type SprStep
    = Start
    | Reading Segment


type alias Segment =
    String


type Answer
    = Yes
    | No
    | Unsure
    | NoAnswerYet


type alias State =
    { answer : Answer
    , step : Step
    , remainingSegments : List TaggedSegment
    , currentSegment : Maybe TaggedSegmentStarted
    , seenSegments : List TaggedSegmentOver
    }


type alias TaggedSegmentStarted =
    { taggedSegment : TaggedSegment, startedAt : Time.Posix }


type alias TaggedSegmentOver =
    { taggedSegment : TaggedSegment, startedAt : Time.Posix, endedAt : Time.Posix }


initState : State
initState =
    { answer = NoAnswerYet
    , step = SPR Start
    , currentSegment = Nothing
    , remainingSegments = []
    , seenSegments = []
    }


decodeSpace : Msg -> Decode.Decoder Msg
decodeSpace msg =
    Decode.map
        (\k ->
            case k of
                " " ->
                    msg

                _ ->
                    NoOp
        )
        (Decode.field "key" Decode.string)


decodeYesNoUnsure : Decode.Decoder Msg
decodeYesNoUnsure =
    Decode.map
        (\k ->
            case k of
                "y" ->
                    UserClickedNextTrial Yes

                "n" ->
                    UserClickedNextTrial No

                "k" ->
                    UserClickedNextTrial Unsure

                _ ->
                    NoOp
        )
        (Decode.field "key" Decode.string)


decodeYesNoUnsureInTraining : Decode.Decoder Msg
decodeYesNoUnsureInTraining =
    Decode.map
        (\k ->
            case k of
                "y" ->
                    UserConfirmedChoice Yes

                "n" ->
                    UserConfirmedChoice No

                "k" ->
                    UserConfirmedChoice Unsure

                _ ->
                    NoOp
        )
        (Decode.field "key" Decode.string)


subscriptions : Logic.Task Trial State -> Sub Msg
subscriptions task =
    case task of
        Logic.Running Logic.Training data ->
            case data.state.step of
                SPR step ->
                    case step of
                        Start ->
                            onKeyDown (decodeSpace (TimestampedMsg UserPressedSpaceToStartParagraph Nothing))

                        Reading _ ->
                            onKeyDown (decodeSpace (TimestampedMsg UserPressedSpaceToReadNextSegment Nothing))

                Feedback ->
                    onKeyDown (decodeSpace (UserClickedNextTrial data.state.answer))

                Question ->
                    onKeyDown decodeYesNoUnsureInTraining

        Logic.Running Logic.Main data ->
            case data.state.step of
                SPR step ->
                    case step of
                        Start ->
                            onKeyDown (decodeSpace (TimestampedMsg UserPressedSpaceToStartParagraph Nothing))

                        Reading _ ->
                            onKeyDown (decodeSpace (TimestampedMsg UserPressedSpaceToReadNextSegment Nothing))

                Question ->
                    onKeyDown decodeYesNoUnsure

                _ ->
                    Sub.none

        _ ->
            Sub.none


saveSprData responseHandler maybeUserId task =
    let
        history =
            Logic.getHistory task

        taskId_ =
            taskId

        callbackHandler =
            responseHandler

        userId =
            maybeUserId |> Maybe.withDefault "recd18l2IBRQNI05y"

        formattedData : List { tag : String, segment : String, startedAt : Int, endedAt : Int, id : String, answer : String }
        formattedData =
            history
                |> List.foldl
                    (\( { id }, { answer, seenSegments } ) acc ->
                        List.map
                            (\{ taggedSegment, startedAt, endedAt } ->
                                { tag = tagToString (Tuple.first taggedSegment)
                                , segment = Tuple.second taggedSegment
                                , startedAt = Time.posixToMillis startedAt
                                , endedAt = Time.posixToMillis endedAt
                                , id = id
                                , answer = answerToString answer
                                }
                            )
                            seenSegments
                            |> List.append acc
                    )
                    []

        summarizedTrialEncoder =
            Encode.list
                (\{ tag, segment, startedAt, endedAt, id, answer } ->
                    Encode.object
                        [ ( "fields"
                          , Encode.object
                                [ ( "sprTrialId", Encode.list Encode.string [ id ] )
                                , ( "answer", Encode.string answer )
                                , ( "sprStartedAt", Encode.int startedAt )
                                , ( "sprEndedAt", Encode.int endedAt )
                                , ( "tag", Encode.string tag )
                                , ( "segment", Encode.string segment )
                                , ( "Task_UID", Encode.list Encode.string [ taskId ] )
                                , ( "userUid", Encode.list Encode.string [ userId ] )
                                ]
                          )
                        ]
                )

        sendInBatch_ =
            Data.sendInBatch summarizedTrialEncoder taskId_ userId formattedData
    in
    Task.attempt callbackHandler sendInBatch_


update : Msg -> Spr model -> ( Spr model, Cmd Msg )
update msg model =
    let
        prevState =
            Logic.getState model.spr |> Maybe.withDefault initState

        currentTrial =
            Logic.getTrial model.spr
    in
    case msg of
        UserConfirmedChoice answer ->
            ( { model | spr = model.spr |> Logic.update { prevState | step = Feedback, answer = answer } }, Cmd.none )

        UserClickedNextTrial newanswer ->
            ( { model | spr = model.spr |> Logic.update { prevState | answer = newanswer } |> Logic.next initState }, Cmd.none )

        UserClickedSaveData ->
            let
                responseHandler =
                    ServerRespondedWithLastRecords
            in
            ( { model | spr = Logic.Loading }, saveSprData responseHandler model.user model.spr )

        ServerRespondedWithLastRecords (Result.Ok _) ->
            ( { model | spr = Logic.NotStarted }, Cmd.none )

        ServerRespondedWithLastRecords (Result.Err _) ->
            ( model, Cmd.none )

        StartMain _ _ ->
            ( { model | spr = Logic.startMain model.spr initState }, Cmd.none )

        TimestampedMsg subMsg timestamp ->
            case subMsg of
                UserPressedSpaceToStartParagraph ->
                    { model
                        | spr =
                            case currentTrial of
                                Nothing ->
                                    model.spr

                                Just tr ->
                                    case tr.taggedSegments of
                                        [] ->
                                            model.spr

                                        x :: xs ->
                                            Logic.update
                                                { prevState
                                                    | step = SPR (Reading "bla)")
                                                    , currentSegment = Just (TaggedSegmentStarted x (Maybe.withDefault (Time.millisToPosix 0) timestamp))
                                                    , remainingSegments = xs
                                                }
                                                model.spr
                    }
                        |> updateWithTime subMsg timestamp model

                UserPressedSpaceToReadNextSegment ->
                    { model
                        | spr =
                            case prevState.remainingSegments of
                                [] ->
                                    Logic.update
                                        { prevState
                                            | step = Question
                                            , seenSegments =
                                                case prevState.currentSegment of
                                                    Just seg ->
                                                        (\{ taggedSegment, startedAt } ->
                                                            { taggedSegment = taggedSegment, startedAt = startedAt, endedAt = Maybe.withDefault (Time.millisToPosix 0) timestamp }
                                                        )
                                                            seg
                                                            :: prevState.seenSegments

                                                    Nothing ->
                                                        prevState.seenSegments
                                        }
                                        model.spr

                                x :: _ ->
                                    Logic.update
                                        { prevState
                                            | step = SPR (Reading "blo")
                                            , currentSegment = Just (TaggedSegmentStarted x (Maybe.withDefault (Time.millisToPosix 0) timestamp))
                                            , remainingSegments = List.tail prevState.remainingSegments |> Maybe.withDefault []
                                            , seenSegments =
                                                case prevState.currentSegment of
                                                    Just seg ->
                                                        (\{ taggedSegment, startedAt } ->
                                                            { taggedSegment = taggedSegment, startedAt = startedAt, endedAt = Maybe.withDefault (Time.millisToPosix 0) timestamp }
                                                        )
                                                            seg
                                                            :: prevState.seenSegments

                                                    Nothing ->
                                                        prevState.seenSegments
                                        }
                                        model.spr
                    }
                        |> updateWithTime subMsg timestamp model

        NoOp ->
            ( model, Cmd.none )

        UserClickedStartTraining ->
            ( { model | spr = Logic.startTraining model.spr }, Cmd.none )


updateWithTime : TimedMsg -> Maybe Time.Posix -> model -> model -> ( model, Cmd Msg )
updateWithTime msg timestamp prevModel newModel =
    if timestamp == Nothing then
        ( prevModel, Task.perform (TimestampedMsg msg) (Task.map Just Time.now) )

    else
        ( newModel
        , Cmd.none
        )


viewTask data trial endTrialMsg =
    case ( data.state.step, data.state.currentSegment ) of
        ( SPR s, Just { taggedSegment } ) ->
            case s of
                Start ->
                    p [ Attr.class "text-bold" ] [ text "Press the space bar to start reading" ]

                Reading _ ->
                    div [ Attr.class "w-max h-max flex flex-col items-center pt-16 pb-16 border-2 text-bold text-lg" ] [ p [ Attr.class "text-lg items-center" ] [ text (Tuple.second taggedSegment) ] ]

        ( SPR _, Nothing ) ->
            p [ Attr.class "text-lg" ] [ text "Press the space bar to start reading" ]

        ( Question, _ ) ->
            div [ Attr.class "w-max h-max flex flex-col items-center pt-16 pb-16 border-2" ]
                [ span [ Attr.class "text-lg" ] [ text trial.question ]
                , text "Press Y for Yes, N for No and K for I don't know"
                ]

        ( Feedback, _ ) ->
            div [ Attr.class "w-max h-max flex flex-col items-center pt-16 pb-16 border-2" ]
                [ View.fromMarkdown trial.feedback
                ]


view : Logic.Task Trial State -> List (Html.Styled.Html Msg)
view task =
    case task of
        Logic.Loading ->
            [ text "Loading... Please don't exit or data may be lost" ]

        Logic.Running Logic.Training data ->
            case data.current of
                Just trial ->
                    [ viewTask data trial UserConfirmedChoice
                    ]

                Nothing ->
                    [ div [ Attr.class "flex flex-col items-center" ]
                        [ View.fromMarkdown data.infos.introToMain
                        , View.button
                            { message = StartMain data.infos data.mainTrials
                            , txt = "Start"
                            , isDisabled = False
                            }
                        ]
                    ]

        Logic.Running Logic.Main data ->
            case data.current of
                Just trial ->
                    [ viewTask data trial UserClickedNextTrial ]

                Nothing ->
                    [ div [ Attr.class "flex flex-col items-center" ] [ View.fromMarkdown data.infos.end, View.button { message = UserClickedSaveData, txt = "Click here to save your data", isDisabled = False } ] ]

        Logic.Err reason ->
            [ text ("I encountered the following error: " ++ reason) ]

        Logic.NotStarted ->
            [ p [] [ text "Thanks for your participation !" ] ]

        Logic.Running Logic.Instructions data ->
            [ View.instructions data.infos.instructions UserClickedStartTraining ]


init infos trials =
    let
        info =
            ExperimentInfo.toDict infos |> Dict.get taskId |> Result.fromMaybe "I couldn't find SPR infos"
    in
    Logic.startIntro info (List.filter (\trial -> trial.isTraining) trials) (List.filter (\trial -> not trial.isTraining) trials) initState


decodeAcceptabilityTrials : Decode.Decoder (List Trial)
decodeAcceptabilityTrials =
    let
        decoder =
            Decode.succeed Trial
                |> required "id" Decode.string
                |> custom (Decode.field "Tagged Paragraph ($CRITIC$, ~SPILL-OVER~)" Decode.string |> Decode.map paragraphToTaggedSegments)
                |> required "Question" Decode.string
                |> optional "isGrammatical" Decode.bool False
                |> optional "isTraining" Decode.bool False
                |> optional "feedback" Decode.string "Missing feedback"
    in
    decodeRecords decoder


getRecords =
    Http.task
        { method = "GET"
        , headers = []
        , url =
            Data.buildQuery
                { app = Data.apps.spacing
                , base = "SPR"
                , view_ = "Pretest"
                }
        , body = Http.emptyBody
        , resolver = Http.stringResolver <| Data.handleJsonResponse <| decodeAcceptabilityTrials
        , timeout = Just 5000
        }


paragraphToTaggedSegments : String -> List TaggedSegment
paragraphToTaggedSegments str =
    String.split "|" str
        |> List.map
            (\seg ->
                if String.contains "$" seg then
                    ( Critic, String.filter ((/=) '$') seg )

                else if String.contains "~" seg then
                    ( SpillOver, String.filter ((/=) '~') seg )

                else
                    ( NoUnit, seg )
            )


answerToString answer =
    case answer of
        Yes ->
            "Yes"

        No ->
            "No"

        Unsure ->
            "I don't know"

        _ ->
            ""


answerFromString str =
    case str of
        "Yes" ->
            Yes

        "No" ->
            No

        "I don't know" ->
            Unsure

        _ ->
            Unsure
