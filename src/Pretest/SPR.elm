module Pretest.SPR exposing (..)

import Browser.Events exposing (onKeyDown)
import Browser.Navigation exposing (Key)
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
import Progressbar exposing (progressBar)
import Task
import Task.Parallel as Para
import Time
import View exposing (unclickableButton)



-- MODEL


type alias Pretest =
    Para.State2 Msg (List Trial) (List ExperimentInfo.Task)


type alias SPR =
    Logic.Task Trial State


type alias Spr model =
    { model
        | spr : Logic.Task Trial State
        , user : Maybe String
        , version : Maybe String
        , key : Key
    }


type alias Trial =
    { id : String
    , taggedSegments : List TaggedSegment
    , question : String
    , isGrammatical : Bool
    , isTraining : Bool
    , feedback : String
    , expectedAnswer : String
    }


type Tag
    = NoUnit
    | Critic
    | SpillOver


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


init infos trials model =
    let
        info =
            ExperimentInfo.toDict infos |> Dict.get (taskId model) |> Result.fromMaybe "I couldn't find SPR infos"
    in
    Logic.startIntro info (List.filter (\trial -> trial.isTraining) trials) (List.filter (\trial -> not trial.isTraining) trials) initState



-- VIEW


view : Logic.Task Trial State -> List (Html.Styled.Html Msg)
view task =
    case task of
        Logic.Loading ->
            [ View.loading ]

        Logic.Running Logic.Training data ->
            case data.current of
                Just trial ->
                    [ viewTask data trial UserConfirmedChoice ]

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
                    [ div
                        [ Attr.class "flow flex flex-col items-center" ]
                        [ View.fromMarkdown data.infos.end
                        , View.button
                            { message = UserClickedSaveData
                            , txt = "Click here when you are ready!"
                            , isDisabled = False
                            }
                        ]
                    ]

        Logic.Err reason ->
            [ text ("I encountered the following error: " ++ reason) ]

        Logic.NotStarted ->
            [ p [] [ text "Thanks for your participation!" ] ]

        Logic.Running Logic.Instructions data ->
            [ View.instructions data.infos UserClickedStartTraining ]


viewTask data trial endTrialMsg =
    case ( data.state.step, data.state.currentSegment ) of
        ( SPR s, Just { taggedSegment } ) ->
            case s of
                Start ->
                    div
                        [ Attr.class "w-max h-max flex flex-col items-center p-16 border-2" ]
                        [ p
                            [ Attr.class "items-center" ]
                            [ text "Press the space bar to start reading" ]
                        ]

                Reading _ ->
                    div
                        [ Attr.class "w-max h-max flex flex-col items-center p-16 border-2 font-bold text-xl" ]
                        [ p
                            [ Attr.class "items-center" ]
                            [ text (Tuple.second taggedSegment) ]
                        ]

        ( SPR _, Nothing ) ->
            div
                [ Attr.class "w-max h-max flex flex-col items-center p-16 border-2" ]
                [ p
                    [ Attr.class "items-center" ]
                    [ text "Press the space bar to start reading" ]
                ]

        ( Question, _ ) ->
            div [ Attr.class "flex flex-col items-center p-16 border-2 flow" ]
                [ span [ Attr.class "font-bold" ] [ text trial.question ]
                , div [ Attr.class "spr-buttons" ]
                    [ unclickableButton "bg-green-500 text-white" "Y = Yes"
                    , unclickableButton "bg-red-500 text-white" "N = No"
                    , unclickableButton "bg-gray-400" "K = I don't know"
                    ]
                ]

        ( Feedback, _ ) ->
            div [ Attr.class "w-max h-max flex flex-col items-center p-16 border-2" ]
                [ View.fromMarkdown trial.feedback ]



-- UPDATE


type Msg
    = NoOp
    | ServerRespondedWithLastRecords (Result.Result Http.Error (List ()))
    | StartMain ExperimentInfo.Task (List Trial)
    | TimestampedMsg TimedMsg (Maybe Time.Posix)
    | UserClickedNextTrial Answer
    | UserClickedSaveData
    | UserConfirmedChoice Answer
    | UserClickedStartTraining
    | HistoryWasSaved (Result Http.Error String)


type TimedMsg
    = UserPressedSpaceToStartParagraph
    | UserPressedSpaceToReadNextSegment


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

        UserClickedNextTrial newAnswer ->
            let
                newModel =
                    { model
                        | spr =
                            model.spr
                                |> Logic.update { prevState | answer = newAnswer }
                                |> Logic.next initState
                    }
            in
            ( newModel, saveData newModel )

        UserClickedSaveData ->
            ( { model | spr = Logic.Loading }
            , if model.version == Just "post" then
                Browser.Navigation.pushUrl model.key "acceptability/instructions"

              else
                Browser.Navigation.pushUrl model.key "sentence-completion"
            )

        ServerRespondedWithLastRecords (Result.Ok _) ->
            ( { model | spr = Logic.NotStarted }, Cmd.none )

        ServerRespondedWithLastRecords (Result.Err reason) ->
            ( { model | spr = Logic.Err (Data.buildErrorMessage reason) }, Cmd.none )

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

        HistoryWasSaved _ ->
            ( model, Cmd.none )


updateWithTime : TimedMsg -> Maybe Time.Posix -> model -> model -> ( model, Cmd Msg )
updateWithTime msg timestamp prevModel newModel =
    if timestamp == Nothing then
        ( prevModel, Task.perform (TimestampedMsg msg) (Task.map Just Time.now) )

    else
        ( newModel
        , Cmd.none
        )



-- SUBSCRIPTIONS


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



-- HTTP


getRecords version =
    Http.task
        { method = "GET"
        , headers = []
        , url =
            Data.buildQuery
                { app = Data.apps.spacing
                , base = "SPR"
                , view_ =
                    case version of
                        Nothing ->
                            "Pretest"

                        Just specifiedVersion ->
                            case specifiedVersion of
                                "pre" ->
                                    "Pretest"

                                "surprise" ->
                                    "Pretest"

                                "post" ->
                                    "Post-test"

                                "post-diff" ->
                                    "Post-test 2"

                                _ ->
                                    "Pretest"

                --}
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
                |> custom (Decode.field "Tagged Paragraph ($CRITIC$, ~SPILL-OVER~)" Decode.string |> Decode.map paragraphToTaggedSegments)
                |> required "Question" Decode.string
                |> optional "isGrammatical" Decode.bool False
                |> optional "isTraining" Decode.bool False
                |> optional "feedback" Decode.string "Missing feedback"
                |> required "expectedAnswer" Decode.string
    in
    decodeRecords decoder


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


saveData model =
    let
        history =
            Logic.getHistory model.spr
                |> List.filter (\( trial, _ ) -> not trial.isTraining)

        userId =
            model.user |> Maybe.withDefault "recd18l2IBRQNI05y"

        version =
            Maybe.withDefault "pre" model.version

        payload =
            updateHistoryEncoder version userId history
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


updateHistoryEncoder : String -> String -> List ( Trial, State ) -> Encode.Value
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


historyEncoder : String -> String -> List ( Trial, State ) -> Encode.Value
historyEncoder version userId history =
    let
        answerField =
            case version of
                "post" ->
                    "SPR_postTest"

                "post-diff" ->
                    "SPR_postTestDiff"

                "surprise" ->
                    "SPR_surprisePostTest"

                _ ->
                    "SPR_preTest"
    in
    Encode.object
        -- airtable does not support JSON columns, so we save giant JSON strings
        [ ( answerField, Encode.string (Encode.encode 0 (Encode.list historyItemEncoder history)) )
        ]


historyItemEncoder : ( Trial, State ) -> Encode.Value
historyItemEncoder ( { expectedAnswer }, { answer, seenSegments } ) =
    Encode.object
        [ ( "answer", Encode.string (answerToString answer) )
        , ( "expectedAnswer", Encode.string expectedAnswer )
        , ( "timings", Encode.string (seenSegmentsToTimingsString seenSegments) )
        , ( "criticalSegmentTime", Encode.string (criticalSegmentTime seenSegments) )
        , ( "spillOverSegmentTime", Encode.string (spillOverSegmentTime seenSegments) )
        ]


tagToString tag =
    case tag of
        NoUnit ->
            "No unit"

        Critic ->
            "Critic"

        SpillOver ->
            "SpillOver"


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


seenSegmentsToTimingsString : List TaggedSegmentOver -> String
seenSegmentsToTimingsString segments =
    segments
        |> List.map segmentToTiming
        |> List.map String.fromInt
        |> String.join ","


criticalSegmentTime : List TaggedSegmentOver -> String
criticalSegmentTime segments =
    let
        isCritical { taggedSegment } =
            case taggedSegment of
                ( Critic, _ ) ->
                    True

                _ ->
                    False
    in
    segments
        |> List.filter isCritical
        |> seenSegmentsToTimingsString


spillOverSegmentTime : List TaggedSegmentOver -> String
spillOverSegmentTime segments =
    let
        isSpillOver { taggedSegment } =
            case taggedSegment of
                ( SpillOver, _ ) ->
                    True

                _ ->
                    False
    in
    segments
        |> List.filter isSpillOver
        |> seenSegmentsToTimingsString


segmentToTiming : TaggedSegmentOver -> Int
segmentToTiming { startedAt, endedAt } =
    Time.posixToMillis endedAt - Time.posixToMillis startedAt



-- INTERNAL


taskId : Maybe String -> String
taskId version =
    case version of
        Nothing ->
            versions.pre

        Just specifiedVersion ->
            case specifiedVersion of
                "post" ->
                    versions.post

                "post-diff" ->
                    versions.postDiff

                "surprise" ->
                    versions.surprise

                _ ->
                    versions.pre


versions =
    { pre = "rec7oxQBDY7rBTRDn", post = "recPo6XGa58q3wfRw", postDiff = "rec1g3y3VAgQwj2sy", surprise = "recfot07G3AMUVK3S" }
