module Pretest.Acceptability exposing (..)

import Browser.Events exposing (onKeyDown)
import Browser.Navigation exposing (Key, pushUrl)
import Data exposing (decodeRecords)
import Delay
import Dict
import ExperimentInfo
import Html.Styled exposing (Html, div, h1, p, pre, span, text)
import Html.Styled.Attributes exposing (class, height, src, width)
import Http
import Json.Decode as Decode
import Json.Decode.Pipeline exposing (..)
import Json.Encode as Encode
import List.Extra
import Logic exposing (Task)
import Ports
import Task
import Time
import User exposing (User)
import View


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


saveAcceptabilityData : (Result.Result Http.Error (List ()) -> msg) -> Model superModel -> Cmd msg
saveAcceptabilityData responseHandler model =
    let
        history =
            Logic.getHistory model.acceptabilityTask

        taskId_ =
            taskId model.version

        callbackHandler =
            responseHandler

        userId =
            model.user |> Maybe.withDefault "recd18l2IBRQNI05y"

        whenNothing =
            Time.millisToPosix 1000000000

        intFromMillis posix =
            Encode.int (Time.posixToMillis (posix |> Maybe.withDefault whenNothing))

        summarizedTrialEncoder =
            Encode.list
                (\( t, s ) ->
                    Encode.object
                        [ ( "fields"
                          , Encode.object
                                [ ( "trialUid", Encode.list Encode.string [ t.uid ] )
                                , ( "userUid", Encode.list Encode.string [ userId ] )
                                , ( "Task_UID", Encode.list Encode.string [ taskId_ ] )
                                , ( "audioStartedAt", intFromMillis s.audioStartedAt )
                                , ( "beepStartedAt", intFromMillis s.beepStartedAt )
                                , ( "audioEndedAt", Encode.int (Time.posixToMillis (s.audioEndedAt |> Maybe.withDefault whenNothing)) )
                                , ( "beepEndedAt", Encode.int (Time.posixToMillis (s.beepEndedAt |> Maybe.withDefault whenNothing)) )
                                , ( "userAnsweredAt", Encode.int (Time.posixToMillis (s.userAnsweredAt |> Maybe.withDefault whenNothing)) )
                                , ( "acceptabilityEval", Encode.string (evalToString s.evaluation) )
                                ]
                          )
                        ]
                )

        sendInBatch_ =
            Data.sendInBatch summarizedTrialEncoder taskId_ userId history
    in
    Task.attempt callbackHandler sendInBatch_


type ErrorBlock
    = FirstDistractorMissing Bool
    | SecondDistractorMissing Bool
    | ThirdDistractorMissing Bool


type Msg
    = UserPressedButton (Maybe Bool)
    | UserPressedButtonWithTimestamp (Maybe Bool) Time.Posix
    | NextStepCinematic Step
    | AudioEnded ( String, Time.Posix )
    | AudioStarted ( String, Time.Posix )
    | StartTraining
    | UserClickedSaveMsg
    | ServerRespondedWithLastRecords (Result.Result Http.Error (List ()))
    | StartMain
    | UserClickedPlayAudio String
    | UserClickedStartTraining
    | NoOp


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


testDistractors =
    List.repeat 16
        { uid = "uid"
        , sentence = "sentence"
        , sentenceType = EmbeddedQuestion
        , trialType = Distractor
        , isGrammatical = True
        , audio = Data.AudioFile "" ""
        }


testTargets =
    List.repeat 4
        { uid = "uid"
        , sentence = "sentence"
        , sentenceType = RelativeClause
        , trialType = Target
        , isGrammatical = True
        , audio = Data.AudioFile "" ""
        }


viewScreen =
    div [ class "flex flex-col w-full h-screen items-center justify-center border-2", Html.Styled.Attributes.id "screen" ]


viewKeys =
    [ div [ class "flex flex-row" ] [ viewKey "f", viewKey "j" ] ]


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


viewInstructions data msg =
    [ h1 [] [ text "Instructions" ]
    , p [ class "w-full w-max-1/3" ] [ View.fromMarkdown data.infos.instructions ]
    , View.button
        { message = msg
        , txt = "String"
        , isDisabled = False
        }
    ]


viewTransition infos msg buttontext =
    div [ class "flex flex-col items-center justify-center" ]
        [ p [] [ View.fromMarkdown infos ]
        , View.button
            { isDisabled = False
            , message = msg
            , txt = buttontext
            }
        ]


grammaticalityToKey : Bool -> String
grammaticalityToKey isGrammatical =
    if isGrammatical then
        "J = ACCEPTABLE: {0}."

    else
        "F = NOT ACCEPTABLE: {0}. The acceptable sentence is: {1}"


view :
    Logic.Task Trial State
    -> List (Html Msg)
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
                    [ div [ class "flex flex-col items-center" ]
                        [ View.button
                            { isDisabled = False
                            , message = StartMain
                            , txt = "That's it for the practice items"
                            }
                        ]
                    ]

                Just trial ->
                    case data.state.step of
                        Init ->
                            [ p [ class "flex flex-col  text-center " ] [ View.fromMarkdown data.infos.instructions_short ]
                            ]

                        End ->
                            [ div [ class "flex flex-col items-center justify-center" ] [ View.fromMarkdown trial.feedback ] ]

                        _ ->
                            [ prompt
                            ]

        Logic.Running Logic.Main data ->
            case data.current of
                Nothing ->
                    [ viewTransition data.infos.end UserClickedSaveMsg "Click to save your answers"
                    ]

                Just _ ->
                    case data.state.step of
                        Init ->
                            [ p [ class "flex flex-col  text-center " ] [ View.fromMarkdown data.infos.trainingWheel ]
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
            [ View.instructions data.infos UserClickedStartTraining ]


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


enumSentenceType : List SentenceType
enumSentenceType =
    [ EmbeddedQuestion, ZeroArticle, AdjectiveAgreement, PresentPerfectOrSimplePast, Conditional, Question, RelativeClause ]


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


start : List ExperimentInfo.Task -> List Trial -> Maybe String -> Logic.Task Trial State
start info trials version =
    let
        relatedInfos =
            Dict.get (taskId version) (ExperimentInfo.toDict info) |> Result.fromMaybe ("I couldn't fetch the value associated with: " ++ taskId version)
    in
    Logic.startIntro relatedInfos
        (List.filter (\datum -> datum.trialType == Training) trials)
        (List.filter (\datum -> datum.trialType /= Training) trials)
        initState


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
    { pre = "recR8areYkKRvQ6lU"
    , post = "recOrxH3ebc5Jhmm4"
    , postDiff = "recN5DtKXo2MEDdvc"
    , surprise = "recTlSt6RDbluzbne"
    }


type TrialType
    = Target
    | Training
    | Distractor


trialTypeToString : TrialType -> String
trialTypeToString trialType =
    case trialType of
        Target ->
            "Target"

        Training ->
            "Training"

        Distractor ->
            "Distractor"


type SentenceType
    = EmbeddedQuestion
    | ZeroArticle
    | AdjectiveAgreement
    | PresentPerfectOrSimplePast
    | Conditional
    | Question
    | RelativeClause


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
    { initState
        | step = Start
    }


type alias History =
    List State


type alias CurrentTrialNumber =
    Int


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


type alias Model supraModel =
    { supraModel
        | acceptabilityTask : Logic.Task Trial State
        , endAcceptabilityDuration : Int
        , key : Key
        , user : Maybe String
        , version : Maybe String
    }


update : Msg -> Model supraModel -> ( Model supraModel, Cmd Msg )
update msg model =
    let
        pState =
            Logic.getState model.acceptabilityTask |> Maybe.withDefault initState

        toNextStep int step =
            Delay.after int (NextStepCinematic step)

        trial =
            Logic.getTrial model.acceptabilityTask |> Maybe.withDefault dumbTrial
    in
    case msg of
        NextStepCinematic step ->
            case step of
                Listening ->
                    ( { model | acceptabilityTask = Logic.update { pState | step = Listening } model.acceptabilityTask }
                    , Delay.after 500 (UserClickedPlayAudio trial.audio.url)
                    )

                Answering ->
                    ( { model | acceptabilityTask = Logic.update { pState | step = Answering } model.acceptabilityTask }, Delay.after trial.timeout (UserPressedButton Nothing) )

                End ->
                    ( { model | acceptabilityTask = Logic.update { pState | step = End } model.acceptabilityTask |> Logic.next pState }
                    , toNextStep 0 Start
                    )

                Start ->
                    ( { model | acceptabilityTask = Logic.update newLoop model.acceptabilityTask }, Delay.after 0 (UserClickedPlayAudio beep) )

                _ ->
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
                | acceptabilityTask =
                    Logic.update
                        { pState
                            | step = End
                            , evaluation = maybeBoolToEvaluation maybeBool
                            , userAnsweredAt = Just timestamp
                        }
                        model.acceptabilityTask
              }
            , toNextStep model.endAcceptabilityDuration End
            )

        AudioEnded ( name, timestamp ) ->
            if name == beep then
                ( { model | acceptabilityTask = Logic.update { pState | beepEndedAt = Just timestamp } model.acceptabilityTask }, Cmd.none )

            else
                ( { model | acceptabilityTask = Logic.update { pState | audioEndedAt = Just timestamp } model.acceptabilityTask }
                , toNextStep 0 Answering
                )

        AudioStarted ( name, timestamp ) ->
            if name == beep then
                ( { model | acceptabilityTask = Logic.update { pState | beepStartedAt = Just timestamp } model.acceptabilityTask }, toNextStep 0 Listening )

            else
                ( { model | acceptabilityTask = Logic.update { pState | audioStartedAt = Just timestamp } model.acceptabilityTask }
                , Cmd.none
                )

        StartTraining ->
            ( model, Cmd.batch [ pushUrl model.key "start" ] )

        StartMain ->
            ( { model | acceptabilityTask = Logic.startMain model.acceptabilityTask initState, endAcceptabilityDuration = 500 }, toNextStep 0 Init )

        UserClickedSaveMsg ->
            let
                responseHandler =
                    ServerRespondedWithLastRecords
            in
            ( { model | acceptabilityTask = Logic.Loading }, saveAcceptabilityData responseHandler model )

        ServerRespondedWithLastRecords (Result.Ok _) ->
            ( { model | acceptabilityTask = Logic.Loading }
            , pushUrl model.key "end"
            )

        UserClickedPlayAudio url ->
            ( model, Ports.playAudio url )

        ServerRespondedWithLastRecords (Result.Err reason) ->
            ( { model | acceptabilityTask = Logic.Err <| Data.buildErrorMessage reason ++ "Please report this error message to yacourt@unice.fr with a nice screenshot!" }, Cmd.none )

        UserClickedStartTraining ->
            ( { model | acceptabilityTask = Logic.startTraining model.acceptabilityTask }, Cmd.none )

        NoOp ->
            ( model, Cmd.none )


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


toAcceptabilityMessage { eventType, name, timestamp } =
    case eventType of
        "SoundStarted" ->
            AudioStarted ( name, Time.millisToPosix timestamp )

        "SoundEnded" ->
            AudioEnded ( name, Time.millisToPosix timestamp )

        _ ->
            NoOp


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



--toEvaluation : String -> Msg
--toEvaluation : String -> Msg


toEvaluation : String -> Msg
toEvaluation x =
    case x of
        "j" ->
            UserPressedButton (Just True)

        "f" ->
            UserPressedButton (Just False)

        _ ->
            NoOp


subscriptions model =
    let
        acceptabilityState =
            Logic.getState model.acceptabilityTask

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
    case model.acceptabilityTask of
        Logic.Running Logic.Training _ ->
            Sub.batch [ listenToInput, Ports.audioEnded toAcceptabilityMessage ]

        Logic.Running Logic.Main _ ->
            Sub.batch [ listenToInput, Ports.audioEnded toAcceptabilityMessage ]

        _ ->
            Sub.none


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
