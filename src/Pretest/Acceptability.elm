module Pretest.Acceptability exposing (..)

import Array
import Data exposing (decodeRecords)
import Dict
import ExperimentInfo
import Html.Styled exposing (Html, div, h1, h3, p, span, text)
import Html.Styled.Attributes exposing (class)
import Http
import Json.Decode as Decode
import Json.Decode.Pipeline exposing (..)
import Logic
import Maybe
import Task
import Time
import Url.Builder exposing (Root(..))
import View


type Msg
    = UserPressedButton (Maybe Bool)
    | UserPressedButtonWithTimestamp (Maybe Bool) Time.Posix
    | NextStepCinematic Step
    | AudioEnded { endedAt : Int, audioName : String }
    | StartTraining
    | UserClickedSaveMsg
    | ServerRespondedWithLastRecords (Result.Result Http.Error (List String))
    | StartMain (List Trial) ExperimentInfo.Task
    | RuntimeShuffledTrials (List ExperimentInfo.Task) (List Trial)


type alias Trial =
    { uid : String
    , sentence : String
    , sentenceType : SentenceType
    , trialType : TrialType
    , isGrammatical : Bool
    , audio : Data.AudioFile
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


view :
    Logic.Task Trial State
    ->
        { saveDataMsg : msg
        , startMainMsg : ExperimentInfo.Task -> List Trial -> msg
        , startTraining : msg
        }
    -> List (Html msg)
view task { startMainMsg, startTraining, saveDataMsg } =
    case task of
        Logic.Intr data ->
            case data.current of
                Nothing ->
                    [ View.introToMain (startMainMsg data.infos data.mainTrials) ]

                Just trial ->
                    case data.state.step of
                        Start ->
                            [ viewScreen []
                            ]

                        Listening ->
                            [ viewScreen [] ]

                        Answering ->
                            [ viewScreen viewKeys ]

                        End ->
                            [ viewScreen [] ]

        Logic.Main data ->
            case data.current of
                Nothing ->
                    [ View.end data.infos.end saveDataMsg "#" ]

                Just trial ->
                    case data.state.step of
                        Start ->
                            [ viewScreen [] ]

                        Listening ->
                            [ viewScreen [] ]

                        Answering ->
                            [ viewScreen viewKeys ]

                        End ->
                            [ viewScreen [] ]

        Logic.Loading ->
            [ text "Loading..." ]

        Logic.NotStarted ->
            [ text "not started" ]

        Logic.Err _ ->
            [ text "error" ]


decodeAcceptabilityTrials : Decode.Decoder (List Trial)
decodeAcceptabilityTrials =
    let
        decoder =
            Decode.succeed Trial
                |> required "id" Decode.string
                |> required "Acceptability Sentence" Decode.string
                |> custom (Decode.field "Sentence type" Decode.string |> Decode.andThen toSentenceType)
                |> custom (Decode.field "Trial type" Decode.string |> Decode.andThen toTrialTypeDecoder)
                |> optional "IsGrammtical" Decode.bool False
                |> required "Acceptability Audio" Data.decodeAudioFiles

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
    , evaluation = False
    , beepEndedAt = Nothing
    , audioEndedAt = Nothing
    , userAnsweredAt = Nothing
    , step = Start
    }


type alias History =
    List State


type alias CurrentTrialNumber =
    Int


type alias State =
    { trialuid : String
    , evaluation : Bool
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


type Evaluation
    = NoEvaluation
    | SentenceCorrect
    | SentenceIncorrect
    | EvaluationTimeOut
