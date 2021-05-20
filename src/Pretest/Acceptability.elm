module Pretest.Acceptability exposing (..)

import Data exposing (decodeRecords)
import Dict
import ExperimentInfo
import Html.Styled exposing (Html, a, div, h1, h3, p, span, text)
import Html.Styled.Attributes exposing (class, height, src, width)
import Http
import Json.Decode as Decode
import Json.Decode.Pipeline exposing (..)
import Json.Encode as Encode
import Logic exposing (Task)
import String.Interpolate exposing (interpolate)
import Task
import Time
import Url.Builder exposing (Root(..))
import View


saveAcceptabilityData : (Result.Result Http.Error (List ()) -> msg) -> Maybe String -> Task Trial State -> Cmd msg
saveAcceptabilityData responseHandler maybeUserId task =
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

        summarizedTrialEncoder =
            Encode.list
                (\( t, s ) ->
                    Encode.object
                        [ ( "fields"
                          , Encode.object
                                [ ( "trialUid", Encode.list Encode.string [ t.uid ] )
                                , ( "userUid", Encode.list Encode.string [ userId ] )
                                , ( "Task_UID", Encode.list Encode.string [ taskId ] )
                                , ( "audioStartedAt", intFromMillis s.audioStartedAt )
                                , ( "beepStartedAt", intFromMillis s.beepStartedAt )
                                , ( "audioEndedAt", Encode.int (Time.posixToMillis (s.audioEndedAt |> Maybe.withDefault whenNothing)) )
                                , ( "beepEndedAt", Encode.int (Time.posixToMillis (s.beepEndedAt |> Maybe.withDefault whenNothing)) )
                                , ( "userAnsweredAt", Encode.int (Time.posixToMillis (s.userAnsweredAt |> Maybe.withDefault whenNothing)) )
                                , ( "evaluation", Encode.string (evalToString s.evaluation) )
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
    | StartMain (List Trial) ExperimentInfo.Task
    | RuntimeShuffledTrials (List ExperimentInfo.Task) (Result.Result ( ErrorBlock, List Trial ) (List (List Trial)))


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
    ->
        { saveDataMsg : msg
        , startMainMsg : ExperimentInfo.Task -> List Trial -> msg
        , startTraining : msg
        }
    -> List (Html msg)
view task { startMainMsg, startTraining, saveDataMsg } =
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
        Logic.Intr data ->
            case data.current of
                Nothing ->
                    [ div [ class "flex flex-col items-center" ]
                        [ View.button
                            { isDisabled = False
                            , message = startMainMsg data.infos data.mainTrials
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

        Logic.Main data ->
            case data.current of
                Nothing ->
                    [ viewTransition data.infos.end saveDataMsg "Click to save your answers"
                    ]

                Just trial ->
                    case data.state.step of
                        Init ->
                            [ p [ class "flex flex-col  text-center " ] [ View.fromMarkdown data.infos.trainingWheel ]
                            ]

                        End ->
                            []

                        _ ->
                            [ prompt ]

        Logic.Loading ->
            [ text "Loading... Please don't exit this page, data may be lost." ]

        Logic.NotStarted ->
            [ text "not started" ]

        Logic.Err reason ->
            [ p [] [ text <| "Oups, I ran into the following error: " ++ reason ]
            ]


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


type alias Block =
    { targetPos : Int
    , d1 : ( Int, Int )
    , d2 : ( Int, Int )
    , d3 : ( Int, Int )
    }


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
                , base = "input"
                , view_ = "Pilote Acceptability"
                }
        , body = Http.emptyBody
        , resolver = Http.stringResolver <| Data.handleJsonResponse <| decodeAcceptabilityTrials
        , timeout = Just 5000
        }


start : List ExperimentInfo.Task -> List Trial -> Logic.Task Trial State
start info trials =
    let
        relatedInfos =
            Dict.get taskId (ExperimentInfo.toDict info) |> Result.fromMaybe ("I couldn't fetch the value associated with: " ++ taskId)
    in
    Logic.startIntro relatedInfos
        (List.filter (\datum -> datum.trialType == Training) trials)
        (List.filter (\datum -> datum.trialType /= Training) trials)
        initState


taskId =
    "recR8areYkKRvQ6lU"


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
