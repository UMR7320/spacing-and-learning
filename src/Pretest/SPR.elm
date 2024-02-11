module Pretest.SPR exposing (..)

import Activity exposing (Activity)
import ActivityInfo exposing (ActivityInfo)
import Browser.Events exposing (onKeyDown)
import Browser.Navigation exposing (Key)
import Data exposing (decodeRecords)
import Html.Styled exposing (..)
import Html.Styled.Attributes as Attr
import Http
import Json.Decode as Decode
import Json.Decode.Pipeline exposing (custom, optional, required)
import Json.Encode as Encode
import Pretest.Version exposing (Version(..))
import Task
import Task.Parallel as Para
import Time
import View exposing (unclickableButton)



-- MODEL


type alias Pretest =
    Para.State2 Msg (List Trial) (List ActivityInfo)


type alias SPR =
    Activity Trial State


type alias Spr model =
    { model
        | spr : SPR
        , user : Maybe String
        , version : Version
        , key : Key
    }


type alias Trial =
    { id : String
    , identifier : String
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


init infos trials version =
    let
        info =
            infos
                |> List.filter (\task -> task.session == Pretest.Version.toSession version && task.name == "Reading test")
                |> List.head
                |> Result.fromMaybe ("Could not find SPR info for version " ++ Pretest.Version.toString version)
    in
    Activity.startIntro info (List.filter (\trial -> trial.isTraining) trials) (List.filter (\trial -> not trial.isTraining) trials) initState



-- VIEW


view : Activity Trial State -> List (Html.Styled.Html Msg)
view task =
    case task of
        Activity.Loading ->
            [ View.loading ]

        Activity.Running Activity.Training data ->
            case data.current of
                Just trial ->
                    [ viewActivity data trial UserConfirmedChoice ]

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

        Activity.Running Activity.Main data ->
            case data.current of
                Just trial ->
                    [ viewActivity data trial UserClickedNextTrial ]

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

        Activity.Err reason ->
            [ text ("I encountered the following error: " ++ reason) ]

        Activity.NotStarted ->
            [ p [] [ text "Thanks for your participation!" ] ]

        Activity.Running Activity.Instructions data ->
            [ View.unsafeInstructions data.infos UserClickedStartTraining ]


viewActivity data trial endTrialMsg =
    case ( data.state.step, data.state.currentSegment ) of
        ( SPR s, Just { taggedSegment } ) ->
            case s of
                Start ->
                    div
                        [ Attr.class "w-max h-max flex flex-col items-center p-16 border-2" ]
                        [ p
                            [ Attr.class "items-center" ]
                            [ text "Press the "
                            , span [ Attr.class "keyboard-key" ] [ text "space" ]
                            , text " bar to start reading"
                            ]
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
                    [ text "Press the "
                    , kbd [] [ text "space" ]
                    , text " bar to start reading"
                    ]
                ]

        ( Question, _ ) ->
            div [ Attr.class "flex flex-col items-center p-16 border-2 flow" ]
                [ span [ Attr.class "font-bold" ] [ text trial.question ]
                , div [ Attr.class "spr-buttons" ]
                    [ unclickableButton
                        "bg-green-500 text-white"
                        [ kbd [] [ text "y" ], text " = Yes" ]
                    , unclickableButton
                        "bg-red-500 text-white"
                        [ kbd [] [ text "n" ], text " = No" ]
                    , unclickableButton
                        "bg-gray-400"
                        [ kbd [] [ text "k" ], text " = I don't know" ]
                    ]
                ]

        ( Feedback, _ ) ->
            div [ Attr.class "w-max h-max flex flex-col items-center p-16 border-2" ]
                [ View.fromUnsafeMarkdown trial.feedback ]



-- UPDATE


type Msg
    = NoOp
    | ServerRespondedWithLastRecords (Result.Result Http.Error (List ()))
    | StartMain ActivityInfo (List Trial)
    | TimestampedMsg TimedMsg (Maybe Time.Posix)
    | UserClickedNextTrial Answer
    | NextTrial Answer Time.Posix
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
            Activity.getState model.spr |> Maybe.withDefault initState

        currentTrial =
            Activity.getTrial model.spr
    in
    case msg of
        UserConfirmedChoice answer ->
            ( { model | spr = model.spr |> Activity.update { prevState | step = Feedback, answer = answer } }, Cmd.none )

        UserClickedNextTrial newAnswer ->
            ( model, Task.perform (NextTrial newAnswer) Time.now )

        NextTrial newAnswer timestamp ->
            let
                newModel =
                    { model
                        | spr =
                            model.spr
                                |> Activity.update { prevState | answer = newAnswer }
                                |> Activity.next timestamp initState
                    }
            in
            ( newModel, saveData newModel )

        UserClickedSaveData ->
            ( { model | spr = Activity.Loading }
            , if model.version == PostTest then
                Browser.Navigation.pushUrl model.key "acceptability/instructions"

              else
                Browser.Navigation.pushUrl model.key "sentence-completion"
            )

        ServerRespondedWithLastRecords (Result.Ok _) ->
            ( { model | spr = Activity.NotStarted }, Cmd.none )

        ServerRespondedWithLastRecords (Result.Err reason) ->
            ( { model | spr = Activity.Err (Data.buildErrorMessage reason) }, Cmd.none )

        StartMain _ _ ->
            ( { model | spr = Activity.startMain model.spr initState }, Cmd.none )

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
                                            Activity.update
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
                                    Activity.update
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
                                    Activity.update
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
            ( { model | spr = Activity.startTraining model.spr }, Cmd.none )

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


subscriptions : Activity Trial State -> Sub Msg
subscriptions task =
    case task of
        Activity.Running Activity.Training data ->
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

        Activity.Running Activity.Main data ->
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
                        PreTest ->
                            "Pretest"

                        Surprise ->
                            "Pretest"

                        PostTest ->
                            "Post-test"

                        PostTestDiff ->
                            "Post-test 2"

                        Unknown _ ->
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
                |> required "Identifier" Decode.string
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
            Activity.getHistory model.spr
                |> List.filter (\( trial, _, timestamp ) -> not trial.isTraining)

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
                    "SPR_preTest"

                PostTest ->
                    "SPR_postTest"

                PostTestDiff ->
                    "SPR_postTestDiff"

                Surprise ->
                    "SPR_surprisePostTest"

                Unknown val ->
                    "SPR_" ++ val
    in
    Encode.object
        -- airtable does not support JSON columns, so we save giant JSON strings
        [ ( answerField, Encode.string (Encode.encode 0 (Encode.list historyItemEncoder history)) )
        ]


historyItemEncoder : ( Trial, State, Time.Posix ) -> Encode.Value
historyItemEncoder ( { id, identifier, expectedAnswer, isGrammatical }, { answer, seenSegments }, timestamp ) =
    Encode.object
        [ ( "trialId", Encode.string id )
        , ( "identifier", Encode.string identifier )
        , ( "answer", Encode.string (answerToString answer) )
        , ( "isGrammatical", Encode.bool isGrammatical )
        , ( "expectedAnswer", Encode.string expectedAnswer )
        , ( "timings", Encode.string (seenSegmentsToTimingsString seenSegments) )
        , ( "criticalSegmentTime", Encode.string (criticalSegmentTime seenSegments) )
        , ( "spillOverSegmentTime", Encode.string (spillOverSegmentTime seenSegments) )
        , ( "answeredAt", Encode.int (Time.posixToMillis timestamp) )
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
