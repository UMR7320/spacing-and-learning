module Pretest.SPR exposing (..)

import Browser.Events exposing (onClick, onKeyDown)
import Data exposing (decodeRecords)
import Dict
import ExperimentInfo
import Html.Styled exposing (..)
import Html.Styled.Attributes as Attr
import Html.Styled.Events as Ev
import Http
import Json.Decode as Decode
import Json.Decode.Pipeline exposing (custom, optional, required)
import Json.Encode as Encode
import Logic
import Random
import Random.List
import String.Interpolate exposing (interpolate)
import Task
import Task.Parallel as Para
import Time
import View


taskId =
    "rec7oxQBDY7rBTRDn"


type alias Pretest =
    Para.State2 Msg (List Trial) (List ExperimentInfo.Task)


attempt =
    Para.attempt2
        { task1 = getRecords
        , task2 = ExperimentInfo.getRecords
        , onUpdates = ServerRespondedWithSomePretestData
        , onFailure = ServerRespondedWithSomeError
        , onSuccess = ServerRespondedWithAllPretestData
        }


type alias Spr model =
    { model
        | spr : Logic.Task Trial State
        , pretest : Para.State2 Msg (List Trial) (List ExperimentInfo.Task)
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
    = UserChoseNewAnswer Answer
    | UserConfirmedChoice
    | UserClickedNextTrial
    | UserClickedSaveData
    | ServerRespondedWithLastRecords (Result.Result Http.Error (List ()))
    | StartMain ExperimentInfo.Task (List Trial)
    | RuntimeShuffledTrials ( List Trial, List ExperimentInfo.Task )
    | NoOp
    | TimestampedMsg TimedMsg (Maybe Time.Posix)
    | ServerRespondedWithSomePretestData (Para.Msg2 (List Trial) (List ExperimentInfo.Task))
    | ServerRespondedWithSomeError Http.Error
    | ServerRespondedWithAllPretestData (List Trial) (List ExperimentInfo.Task)


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
    , seenSegments : List (Maybe TaggedSegmentOver)
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


subscriptions : Maybe State -> Sub Msg
subscriptions state =
    case state of
        Nothing ->
            Sub.none

        Just s ->
            case s.step of
                SPR step ->
                    case step of
                        Start ->
                            onKeyDown (decodeSpace (TimestampedMsg UserPressedSpaceToStartParagraph Nothing))

                        Reading _ ->
                            onKeyDown (decodeSpace (TimestampedMsg UserPressedSpaceToReadNextSegment Nothing))

                Feedback ->
                    onKeyDown (decodeSpace UserClickedNextTrial)

                Question ->
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

        whenNothing =
            Time.millisToPosix 1000000000

        intFromMillis posix =
            Encode.int (Time.posixToMillis (posix |> Maybe.withDefault whenNothing))

        formattedData =
            history |> List.map (\( { id }, { answer, seenSegments } ) -> { id = id, answer = answerToString answer, seenSegments = extract seenSegments })

        extract seenSegments =
            List.foldl
                (\seg acc ->
                    case seg of
                        Just { taggedSegment, startedAt, endedAt } ->
                            String.Interpolate.interpolate "{\"tag\": \"{0}\", \"segment\": \"{1}\", \"startedAt\": {2}, \"endedAt\": {3} }"
                                [ tagToString (Tuple.first taggedSegment)
                                , Tuple.second taggedSegment
                                , String.fromInt (Time.posixToMillis startedAt)
                                , String.fromInt (Time.posixToMillis endedAt)
                                ]
                                :: acc

                        Nothing ->
                            acc
                )
                []
                seenSegments
                |> String.join ","
                |> String.append "["
                |> (\f a b -> f b a) String.append "]"

        summarizedTrialEncoder =
            Encode.list
                (\{ id, answer, seenSegments } ->
                    Encode.object
                        [ ( "fields"
                          , Encode.object
                                [ ( "sprTrialId", Encode.list Encode.string [ id ] )
                                , ( "answer", Encode.string answer )
                                , ( "blob", Encode.string seenSegments )
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
        UserChoseNewAnswer newAnswer ->
            ( { model | spr = Logic.update { prevState | answer = newAnswer } model.spr }, Cmd.none )

        UserConfirmedChoice ->
            ( { model | spr = Logic.update { prevState | step = Feedback } model.spr }, Cmd.none )

        UserClickedNextTrial ->
            ( { model | spr = Logic.next initState model.spr }, Cmd.none )

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

        StartMain task trials ->
            ( { model | spr = Logic.startMain model.spr initState }, Cmd.none )

        RuntimeShuffledTrials ( infos, trials ) ->
            ( { model | spr = init trials infos }, Cmd.none )

        ServerRespondedWithSomePretestData downloaded ->
            let
                ( nextState, nextCmd ) =
                    Para.update2 model.pretest downloaded
            in
            ( { model | pretest = nextState }, nextCmd )

        ServerRespondedWithSomeError error ->
            ( { model | spr = Logic.Err (Data.buildErrorMessage error) }, Cmd.none )

        ServerRespondedWithAllPretestData trials infos ->
            ( model, Random.generate RuntimeShuffledTrials (Random.pair (Random.List.shuffle trials) (Random.constant infos)) )

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
                                                Maybe.map
                                                    (\{ taggedSegment, startedAt } ->
                                                        { taggedSegment = taggedSegment, startedAt = startedAt, endedAt = Maybe.withDefault (Time.millisToPosix 0) timestamp }
                                                    )
                                                    prevState.currentSegment
                                                    :: prevState.seenSegments
                                        }
                                        model.spr

                                x :: xs ->
                                    Logic.update
                                        { prevState
                                            | step = SPR (Reading "blo")
                                            , currentSegment = Just (TaggedSegmentStarted x (Maybe.withDefault (Time.millisToPosix 0) timestamp))
                                            , remainingSegments = List.tail prevState.remainingSegments |> Maybe.withDefault []
                                            , seenSegments =
                                                Maybe.map
                                                    (\{ taggedSegment, startedAt } ->
                                                        { taggedSegment = taggedSegment, startedAt = startedAt, endedAt = Maybe.withDefault (Time.millisToPosix 0) timestamp }
                                                    )
                                                    prevState.currentSegment
                                                    :: prevState.seenSegments
                                        }
                                        model.spr
                    }
                        |> updateWithTime subMsg timestamp model

        NoOp ->
            ( model, Cmd.none )


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
                    p [ Attr.class "text-bold" ] [ text "Press space to start reading" ]

                Reading segment ->
                    div [ Attr.class "w-max h-max flex flex-col items-center pt-16 pb-16 border-2" ] [ p [ Attr.class "text-lg items-center" ] [ text (Tuple.second taggedSegment) ] ]

        ( SPR s, Nothing ) ->
            p [ Attr.class "text-lg" ] [ text "Press space to start reading" ]

        ( Question, _ ) ->
            let
                yes =
                    answerToString Yes

                no =
                    answerToString No

                unsure =
                    answerToString Unsure

                value =
                    answerToString data.state.answer
            in
            div [ Attr.class "w-max h-max flex flex-col items-center pt-16 pb-16 border-2" ]
                [ Html.Styled.fieldset [ Attr.class "flex flex-col" ]
                    [ legend [] [ text trial.question ]
                    , div [ Attr.class "flex flex-row" ]
                        [ input
                            [ Attr.type_ "radio"
                            , Attr.id yes
                            , Attr.value yes
                            , Attr.checked (value == yes)
                            , Ev.onClick (UserChoseNewAnswer (answerFromString yes))
                            ]
                            []
                        , label [ Attr.for yes ] [ text yes ]
                        ]
                    , div []
                        [ input
                            [ Attr.type_ "radio"
                            , Attr.id no
                            , Attr.value no
                            , Attr.checked (value == no)
                            , Ev.onClick (UserChoseNewAnswer (answerFromString no))
                            ]
                            []
                        , label [ Attr.for no ] [ text no ]
                        ]
                    , div []
                        [ input
                            [ Attr.type_ "radio"
                            , Attr.id unsure
                            , Attr.value unsure
                            , Attr.checked (value == unsure)
                            , Ev.onClick (UserChoseNewAnswer (answerFromString unsure))
                            ]
                            []
                        , label [ Attr.for no, Attr.value value ] [ text unsure ]
                        ]
                    ]
                , View.button
                    { message = endTrialMsg
                    , txt =
                        if endTrialMsg == UserClickedNextTrial then
                            "Next item"

                        else
                            "Click here to see the feedback"
                    , isDisabled = String.isEmpty value
                    }
                ]

        ( Feedback, _ ) ->
            div [ Attr.class "w-max h-max flex flex-col items-center pt-16 pb-16 border-2" ] [ View.fromMarkdown trial.feedback, View.button { message = UserClickedNextTrial, txt = "Next item", isDisabled = False } ]


view : Logic.Task Trial State -> List (Html.Styled.Html Msg)
view task =
    case task of
        Logic.Loading ->
            [ text "Loading... Please don't exit or data may be lost" ]

        Logic.Intr data ->
            case data.current of
                Just trial ->
                    [ p [] [ View.fromMarkdown data.infos.instructions ]
                    , viewTask data trial UserConfirmedChoice
                    ]

                Nothing ->
                    [ div [ Attr.class "flex flex-col items-center" ]
                        [ text data.infos.introToMain
                        , View.button
                            { message = StartMain data.infos data.mainTrials
                            , txt = "Start"
                            , isDisabled = False
                            }
                        ]
                    ]

        Logic.Main data ->
            case data.current of
                Just trial ->
                    [ viewTask data trial UserClickedNextTrial ]

                Nothing ->
                    [ div [ Attr.class "flex flex-col items-center" ] [ text data.infos.end, View.button { message = UserClickedSaveData, txt = "Click here to save your data", isDisabled = False } ] ]

        Logic.Err reason ->
            [ text ("I encountered the following error: " ++ reason) ]

        Logic.NotStarted ->
            [ p [] [ text "Thanks for your participation !" ] ]


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
