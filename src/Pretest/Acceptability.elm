module Pretest.Acceptability exposing (..)

import Browser.Events exposing (onKeyDown)
import Browser.Navigation exposing (Key, pushUrl)
import Data exposing (decodeRecords)
import Delay
import ExperimentInfo exposing (Session(..))
import Html.Styled exposing (Html, div, p, pre, span, text)
import Html.Styled.Attributes exposing (class, height, src, width)
import Http
import Json.Decode as Decode
import Json.Decode.Pipeline exposing (..)
import Json.Encode as Encode
import List.Extra
import Logic
import Ports
import Pretest.Version exposing (Version(..))
import Task
import Time
import View



-- MODEL


type alias Trial =
    { uid : String
    , sentence : String
    , sentenceType : SentenceType
    , trialType : TrialType
    , isGrammatical : Bool
    , audio : Data.AudioFile
    , feedback : String
    , timeout : Int
    }


type TrialType
    = Target
    | Training
    | Distractor


type SentenceType
    = EmbeddedQuestion
    | ZeroArticle
    | AdjectiveAgreement
    | PresentPerfectOrSimplePast
    | Conditional
    | Question
    | RelativeClause


type alias State =
    { trialuid : String
    , evaluation : Evaluation
    , beepStartedAt : Maybe Time.Posix
    , audioStartedAt : Maybe Time.Posix
    , beepEndedAt : Maybe Time.Posix
    , audioEndedAt : Maybe Time.Posix
    , userAnsweredAt : Maybe Time.Posix
    , step : Step
    }


type alias History =
    List State


type Step
    = Start
    | Listening
    | Answering
    | End
    | Init


type Evaluation
    = NoEvaluation
    | SentenceCorrect
    | SentenceIncorrect
    | EvaluationTimeOut


type alias Acceptability =
    Logic.Activity Trial State


type alias Model supraModel =
    { supraModel
        | acceptability : Acceptability
        , endAcceptabilityDuration : Int
        , key : Key
        , user : Maybe String
        , version : Version
    }


dumbTrial =
    { uid = "dumbId"
    , sentence = "dumbSentence"
    , sentenceType = EmbeddedQuestion
    , trialType = Target
    , isGrammatical = False
    , audio = Data.AudioFile "" ""
    , feedback = "String"
    , timeout = 5000
    }


initState : State
initState =
    { trialuid = "defaulttrialuid"
    , evaluation = NoEvaluation
    , beepEndedAt = Nothing
    , beepStartedAt = Nothing
    , audioStartedAt = Nothing
    , audioEndedAt = Nothing
    , userAnsweredAt = Nothing
    , step = Init
    }


newLoop : State
newLoop =
    { initState | step = Start }


start : List ExperimentInfo.Activity -> List Trial -> Version -> Logic.Activity Trial State
start info trials version =
    Logic.startIntro
        (ExperimentInfo.activityInfo info (Pretest.Version.toSession version) "Listening test")
        (List.filter (\datum -> datum.trialType == Training) trials)
        (List.filter (\datum -> datum.trialType /= Training) trials)
        initState



-- VIEW


view : Logic.Activity Trial State -> List (Html Msg)
view task =
    let
        prompt =
            div [ class "flex flex-col items-center justify-center" ]
                [ Html.Styled.img
                    [ src "/acceptability.png"
                    , class "items-center justify-center"
                    , width 500
                    , height 500
                    ]
                    []
                ]
    in
    case task of
        Logic.Running Logic.Training data ->
            case data.current of
                Nothing ->
                    [ div [ class "flex flex-col items-center flow" ]
                        [ View.fromMarkdown data.infos.introToMain
                        , View.button
                            { isDisabled = False
                            , message = StartMain
                            , txt = "Continue"
                            }
                        ]
                    ]

                Just trial ->
                    case data.state.step of
                        Init ->
                            [ View.fromUnsafeMarkdown data.infos.instructions_short ]

                        End ->
                            [ div [ class "flex flex-col items-center justify-center" ] [ View.fromUnsafeMarkdown trial.feedback ] ]

                        _ ->
                            [ prompt
                            ]

        Logic.Running Logic.Main data ->
            case data.current of
                Nothing ->
                    [ viewTransition
                        data.infos.end
                        UserClickedEndActivity
                        "Continue"
                    ]

                Just _ ->
                    case data.state.step of
                        Init ->
                            [ p [ class "flex flex-col  text-center " ] [ View.fromUnsafeMarkdown data.infos.trainingWheel ]
                            ]

                        End ->
                            []

                        _ ->
                            [ prompt ]

        Logic.Loading ->
            [ View.loading ]

        Logic.NotStarted ->
            [ text "not started" ]

        Logic.Err reason ->
            [ pre [] [ text reason ]
            ]

        Logic.Running Logic.Instructions data ->
            [ View.unsafeInstructions data.infos UserClickedStartTraining ]


viewScreen =
    div
        [ class "flex flex-col w-full h-screen items-center justify-center border-2"
        , Html.Styled.Attributes.id "screen"
        ]


viewKeys =
    [ div
        [ class "flex flex-row" ]
        [ viewKey "f", viewKey "j" ]
    ]


viewKey label =
    let
        bg =
            if label == "j" then
                "bg-green-500"

            else if "f" == label then
                "bg-red-500"

            else
                ""
    in
    div [ class <| "flex flex-col h-12 w-12 m-4 items-center justify-center border-2 rounded-lg " ++ bg ]
        [ span [ class "flex flex-col  items-center text-2xl font-bold text-black" ]
            [ text label
            ]
        ]


viewTransition infos msg buttontext =
    div [ class "flex flex-col items-center justify-center" ]
        [ p [] [ View.fromUnsafeMarkdown infos ] ]



-- UPDATE


type Msg
    = UserPressedButton (Maybe Bool)
    | UserPressedButtonWithTimestamp (Maybe Bool) Time.Posix
    | NextStepCinematic Step
    | NextStepCinematicWithTimestamp Step Time.Posix
    | AudioEnded ( String, Time.Posix )
    | AudioStarted ( String, Time.Posix )
    | StartTraining
    | UserClickedEndActivity
    | StartMain
    | PlayAudio String
    | UserClickedStartTraining
    | HistoryWasSaved (Result Http.Error String)
    | NoOp


update : Msg -> Model supraModel -> ( Model supraModel, Cmd Msg )
update msg model =
    let
        pState =
            Logic.getState model.acceptability |> Maybe.withDefault initState

        toNextStep int step =
            Delay.after int (NextStepCinematic step)

        trial =
            Logic.getTrial model.acceptability |> Maybe.withDefault dumbTrial
    in
    case msg of
        NextStepCinematic step ->
            ( model, Task.perform (NextStepCinematicWithTimestamp step) Time.now )

        NextStepCinematicWithTimestamp step timestamp ->
            case step of
                Listening ->
                    ( { model
                        | acceptability =
                            Logic.update { pState | step = Listening } model.acceptability
                      }
                    , Delay.after 500 (PlayAudio trial.audio.url)
                    )

                Answering ->
                    ( { model
                        | acceptability =
                            Logic.update { pState | step = Answering } model.acceptability
                      }
                    , Delay.after trial.timeout (UserPressedButton Nothing)
                    )

                End ->
                    let
                        newModel =
                            { model
                                | acceptability =
                                    Logic.update { pState | step = End } model.acceptability
                                        |> Logic.next timestamp pState
                            }
                    in
                    ( newModel
                    , Cmd.batch
                        [ toNextStep 0 Start
                        , saveData newModel
                        ]
                    )

                Start ->
                    ( { model
                        | acceptability =
                            Logic.update newLoop model.acceptability
                      }
                    , Delay.after 0 (PlayAudio beep)
                    )

                Init ->
                    ( model, Cmd.none )

        UserPressedButton maybeBool ->
            let
                forward =
                    if pState.step == Answering then
                        Task.perform (\timestamp -> UserPressedButtonWithTimestamp maybeBool timestamp) Time.now

                    else
                        Cmd.none
            in
            ( model, forward )

        UserPressedButtonWithTimestamp maybeBool timestamp ->
            ( { model
                | acceptability =
                    Logic.update
                        { pState
                            | step = End
                            , evaluation = maybeBoolToEvaluation maybeBool
                            , userAnsweredAt = Just timestamp
                        }
                        model.acceptability
              }
            , toNextStep model.endAcceptabilityDuration End
            )

        AudioEnded ( name, timestamp ) ->
            if name == beep then
                ( { model
                    | acceptability =
                        Logic.update { pState | beepEndedAt = Just timestamp } model.acceptability
                  }
                , Cmd.none
                )

            else
                ( { model
                    | acceptability =
                        Logic.update { pState | audioEndedAt = Just timestamp } model.acceptability
                  }
                , toNextStep 0 Answering
                )

        AudioStarted ( name, timestamp ) ->
            if name == beep then
                ( { model | acceptability = Logic.update { pState | beepStartedAt = Just timestamp } model.acceptability }, toNextStep 0 Listening )

            else
                ( { model | acceptability = Logic.update { pState | audioStartedAt = Just timestamp } model.acceptability }
                , Cmd.none
                )

        StartTraining ->
            ( model, Cmd.batch [ pushUrl model.key "start" ] )

        StartMain ->
            ( { model | acceptability = Logic.startMain model.acceptability initState, endAcceptabilityDuration = 500 }, toNextStep 0 Init )

        UserClickedEndActivity ->
            ( model, pushUrl model.key "end" )

        PlayAudio url ->
            ( model, Ports.playAudio url )

        UserClickedStartTraining ->
            ( { model | acceptability = Logic.startTraining model.acceptability }, Cmd.none )

        HistoryWasSaved _ ->
            ( model, Cmd.none )

        NoOp ->
            ( model, Cmd.none )



-- SUBSCRIPTIONS


subscriptions model =
    let
        acceptabilityState =
            Logic.getState model.acceptability

        listenToInput : Sub Msg
        listenToInput =
            case acceptabilityState of
                Just state ->
                    if state.step == Answering then
                        onKeyDown keyDecoder

                    else if state.step == Init then
                        onKeyDown decodeSpace

                    else
                        Sub.none

                Nothing ->
                    Sub.none
    in
    case model.acceptability of
        Logic.Running Logic.Training _ ->
            Sub.batch [ listenToInput, Ports.audioEnded toAcceptabilityMessage ]

        Logic.Running Logic.Main _ ->
            Sub.batch [ listenToInput, Ports.audioEnded toAcceptabilityMessage ]

        _ ->
            Sub.none


keyDecoder : Decode.Decoder Msg
keyDecoder =
    Decode.map toEvaluation (Decode.field "key" Decode.string)


decodeSpace : Decode.Decoder Msg
decodeSpace =
    Decode.map
        (\k ->
            case k of
                " " ->
                    NextStepCinematic Start

                _ ->
                    NoOp
        )
        (Decode.field "key" Decode.string)


toEvaluation : String -> Msg
toEvaluation x =
    case x of
        "j" ->
            UserPressedButton (Just True)

        "f" ->
            UserPressedButton (Just False)

        _ ->
            NoOp


toAcceptabilityMessage { eventType, name, timestamp } =
    case eventType of
        "SoundStarted" ->
            AudioStarted ( name, Time.millisToPosix timestamp )

        "SoundEnded" ->
            AudioEnded ( name, Time.millisToPosix timestamp )

        _ ->
            NoOp



-- HTTP


getRecords =
    Http.task
        { method = "GET"
        , headers = []
        , url =
            Data.buildQuery
                { app = Data.apps.spacing
                , base = "acceptability"
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
                |> required "Acceptability Sentence" Decode.string
                |> custom (Decode.field "Sentence type" Decode.string |> Decode.andThen toSentenceType)
                |> custom (Decode.field "Trial type" Decode.string |> Decode.andThen toTrialTypeDecoder)
                |> optional "IsGrammatical" Decode.bool False
                |> required "Acceptability Audio" Data.decodeAudioFiles
                |> optional "Acceptability Feedback" Decode.string "no feedback"
                |> required "Timeout" Decode.int

        toTrialTypeDecoder str =
            case str of
                "Target" ->
                    Decode.succeed Target

                "Training" ->
                    Decode.succeed Training

                "Distractor" ->
                    Decode.succeed Distractor

                _ ->
                    Decode.fail <| "I couldn't map " ++ str ++ " to TrialType"

        toSentenceType str =
            case str of
                "Embedded Question" ->
                    Decode.succeed EmbeddedQuestion

                "Zero article" ->
                    Decode.succeed ZeroArticle

                "Adjective agreement" ->
                    Decode.succeed AdjectiveAgreement

                "Present perfect/simple past" ->
                    Decode.succeed PresentPerfectOrSimplePast

                "Conditional" ->
                    Decode.succeed Conditional

                "Question" ->
                    Decode.succeed Question

                "Relative clause" ->
                    Decode.succeed RelativeClause

                _ ->
                    Decode.fail ("I couldn't find the corresponding SentenceType for this string :" ++ str)
    in
    decodeRecords decoder


saveData model =
    let
        history =
            Logic.getHistory model.acceptability

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
historyEncoder version _ history =
    let
        answerField =
            case version of
                PreTest ->
                    "Acceptability_preTest"

                PostTest ->
                    "Acceptability_postTest"

                PostTestDiff ->
                    "Acceptability_postTestDiff"

                Surprise ->
                    "Acceptability_surprisePostTest"

                Unknown val ->
                    "Acceptability_" ++ val
    in
    Encode.object
        -- airtable does not support JSON columns, so we save giant JSON strings
        [ ( answerField, Encode.string (Encode.encode 0 (Encode.list historyItemEncoder history)) )
        ]


historyItemEncoder : ( Trial, State, Time.Posix ) -> Encode.Value
historyItemEncoder ( trial, state, _ ) =
    let
        toMillisOrZero timestamp =
            timestamp
                |> Maybe.map Time.posixToMillis
                |> Maybe.withDefault 0
    in
    Encode.object
        [ ( "trialUid", Encode.string trial.uid )
        , ( "audioStartedAt", Encode.int (state.audioStartedAt |> toMillisOrZero) )
        , ( "beepStartedAt", Encode.int (state.beepStartedAt |> toMillisOrZero) )
        , ( "audioEndedAt", Encode.int (state.audioEndedAt |> toMillisOrZero) )
        , ( "beepEndedAt", Encode.int (state.beepEndedAt |> toMillisOrZero) )
        , ( "userAnsweredAt", Encode.int (state.userAnsweredAt |> toMillisOrZero) )
        , ( "acceptabilityEval", Encode.string (evalToString state.evaluation) )
        , ( "sentenceType", Encode.string (sentenceTypeToString trial.sentenceType) )
        , ( "trialType", Encode.string (trialTypeToString trial.trialType) )
        , ( "isGrammatical", Encode.bool trial.isGrammatical )
        ]



-- INTERNALS


type ErrorBlock
    = FirstDistractorMissing Bool
    | SecondDistractorMissing Bool
    | ThirdDistractorMissing Bool


trialTypeToString : TrialType -> String
trialTypeToString trialType =
    case trialType of
        Target ->
            "Target"

        Training ->
            "Training"

        Distractor ->
            "Distractor"


sentenceTypeToString : SentenceType -> String
sentenceTypeToString sentenceType =
    case sentenceType of
        EmbeddedQuestion ->
            "Embedded Question"

        ZeroArticle ->
            "Zero article"

        AdjectiveAgreement ->
            "Adjective agreement"

        PresentPerfectOrSimplePast ->
            "Present perfect/simple past"

        Conditional ->
            "Conditional"

        Question ->
            "Question"

        RelativeClause ->
            "Relative clause"


maybeBoolToEvaluation : Maybe Bool -> Evaluation
maybeBoolToEvaluation maybeBool =
    case maybeBool of
        Nothing ->
            EvaluationTimeOut

        Just True ->
            SentenceCorrect

        Just False ->
            SentenceIncorrect


evalToString : Evaluation -> String
evalToString eval =
    case eval of
        NoEvaluation ->
            "No Eval"

        SentenceCorrect ->
            "Correct"

        SentenceIncorrect ->
            "Incorrect"

        EvaluationTimeOut ->
            "Timeout"


organizeAcceptabilityTrials : List Trial -> List Trial -> Result.Result ( ErrorBlock, List Trial ) (List (List Trial))
organizeAcceptabilityTrials targets distractors =
    organizeAcceptabilityTrialsHelper targets distractors []


organizeAcceptabilityTrialsHelper : List Trial -> List Trial -> List (List Trial) -> Result.Result ( ErrorBlock, List Trial ) (List (List Trial))
organizeAcceptabilityTrialsHelper targets distractors output =
    -- Acceptability trials must be organized in sequence of blocks containing exactly one target and 3 distractors belonging to 3 different sentence type.
    -- After shuffling all the trials, this function is used create the proper sequence.
    -- Because the target can't be at the first position of a sequence, we have to swap the position of the target with one of the following distractors. TODO
    -- En gros, ça va marcher tant qu'il y a le bon nombre d'items mais s'il devait y avoir un déséquilibre, cela créera une recursion infinie.
    -- C'est le pire code de l'enfer, désolé si quelqu'un d'autre que moi voit ce massacre.
    let
        nextGrammaticalSentence buff dis =
            dis.isGrammatical && not (List.member dis.sentenceType (getSentenceTypes buff))

        --not (List.member dis.sentenceType (getSentenceTypes buff))
        --&& not (List.member dis.sentenceType (whichSentenceTypes buff))
        nextUngrammaticalSentence buff dis =
            not dis.isGrammatical && not (List.member dis.sentenceType (getSentenceTypes buff))

        --List.member dis.sentenceType (getSentenceTypes buff) |> not
        findFirstGrammaticalDistractor =
            List.Extra.find (nextGrammaticalSentence []) distractors

        findSecondGrammaticalDistractor firstDistractor =
            List.Extra.find (nextGrammaticalSentence firstDistractor) (removesItems firstDistractor distractors)

        findThirdGrammaticalDistractor firstDistractor secondDistractor =
            List.Extra.find (nextGrammaticalSentence [ firstDistractor, secondDistractor ]) (removesItems [ firstDistractor, secondDistractor ] distractors)

        firstUnGrammaticalDistractor =
            List.Extra.find (nextUngrammaticalSentence []) distractors

        findSecondUnGrammaticalDistractor firstDistractor =
            removesItems [ firstDistractor ] distractors
                |> List.Extra.find (nextUngrammaticalSentence [ firstDistractor ])

        findThirdUnGrammaticalDistractor firstDistractor secondDistractor =
            removesItems [ firstDistractor, secondDistractor ] distractors
                |> List.Extra.find (nextUngrammaticalSentence [ firstDistractor, secondDistractor ])

        buildBlock target =
            if target.isGrammatical then
                firstUnGrammaticalDistractor
                    |> Result.fromMaybe ( FirstDistractorMissing False, [ target ] )
                    |> Result.andThen
                        (\distractorFound ->
                            findSecondGrammaticalDistractor [ distractorFound ]
                                |> Result.fromMaybe
                                    ( SecondDistractorMissing True
                                    , [ target, distractorFound ]
                                    )
                                |> Result.andThen
                                    (\secondDistractorFound ->
                                        findThirdUnGrammaticalDistractor distractorFound secondDistractorFound
                                            |> Result.fromMaybe
                                                ( ThirdDistractorMissing False
                                                , [ target, distractorFound, secondDistractorFound ]
                                                )
                                            |> Result.andThen
                                                (\thirdDistractorFound ->
                                                    Result.Ok
                                                        { target = target
                                                        , firstDistractor = distractorFound
                                                        , secondDistractor = secondDistractorFound
                                                        , thirdDistractor = thirdDistractorFound
                                                        , remainingDistractors = removesItems [ distractorFound, secondDistractorFound, thirdDistractorFound ] distractors
                                                        }
                                                )
                                    )
                        )

            else
                findFirstGrammaticalDistractor
                    |> Result.fromMaybe ( FirstDistractorMissing True, [ target ] )
                    |> Result.andThen
                        (\distractorFound ->
                            findSecondUnGrammaticalDistractor distractorFound
                                |> Result.fromMaybe ( SecondDistractorMissing False, [ target, distractorFound ] )
                                |> Result.andThen
                                    (\secondDistractorFound ->
                                        findThirdGrammaticalDistractor distractorFound secondDistractorFound
                                            |> Result.fromMaybe ( ThirdDistractorMissing True, [ target, distractorFound, secondDistractorFound ] )
                                            |> Result.andThen
                                                (\thirdDistractorFound ->
                                                    Result.Ok
                                                        { target = target
                                                        , firstDistractor = distractorFound
                                                        , secondDistractor = secondDistractorFound
                                                        , thirdDistractor = thirdDistractorFound
                                                        , remainingDistractors = removesItems [ distractorFound, secondDistractorFound, thirdDistractorFound ] distractors
                                                        }
                                                )
                                    )
                        )
    in
    case targets of
        [] ->
            Result.Ok (output ++ [ distractors ])

        x :: xs ->
            case buildBlock x of
                Result.Err (( _, blockSoFar ) as err) ->
                    organizeAcceptabilityTrialsHelper xs (removesItems blockSoFar distractors) (blockSoFar :: output)

                Result.Ok { target, firstDistractor, secondDistractor, thirdDistractor, remainingDistractors } ->
                    let
                        block =
                            [ target, firstDistractor, secondDistractor, thirdDistractor ]
                    in
                    organizeAcceptabilityTrialsHelper xs remainingDistractors (block :: output)


nextNewSentenceType buff dis =
    List.member dis.sentenceType (getSentenceTypes buff) |> not


beep =
    "https://dl.airtable.com/.attachments/b000c72585c5f5145828b1cf3916c38d/88d9c821/beep.mp3"


removesItemsHelp : List a -> List a -> List a -> List a
removesItemsHelp items ls acc =
    case ls of
        [] ->
            List.reverse acc

        x :: xs ->
            if List.member x items then
                removesItemsHelp items xs acc

            else
                removesItemsHelp items xs (x :: acc)


removesItems : List a -> List a -> List a
removesItems items ls =
    removesItemsHelp items ls []


isNextSentence : { a | sentenceType : SentenceType } -> List { b | sentenceType : SentenceType } -> Bool
isNextSentence dis blockBuffer =
    List.member dis.sentenceType (getSentenceTypes blockBuffer) |> not


getSentenceTypes : List { a | sentenceType : SentenceType } -> List SentenceType
getSentenceTypes sentences =
    List.map .sentenceType sentences
