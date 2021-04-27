module Pretest.SPR exposing (..)

import Data exposing (decodeRecords)
import Dict
import ExperimentInfo
import Html.Styled exposing (text)
import Http
import Json.Decode as Decode
import Json.Decode.Pipeline exposing (custom, optional, required)
import Json.Encode as Encode
import Logic
import Time


view =
    text "spr"


taskId =
    "rec7oxQBDY7rBTRDn"



-- Types


type alias Trial =
    { id : String
    , taggedSegments : List TaggedSegment
    , question : String
    , isGrammatical : Bool
    , isTraining : Bool
    }


type Msg
    = UserPressedSpaceToStartTraining Time.Posix
    | UserPressedSpaceToReadNextSegment Time.Posix
    | UserChoseNewAnswer Answer
    | UserConfirmedChoice
    | UserClickedNextTrial Time.Posix
    | UserClickedSaveData
    | ServerRespondedWithLastRecords (Result.Result Http.Error (List ()))
    | StartMain ExperimentInfo.Task (List Trial)
    | RuntimeShuffledTrials (List ExperimentInfo.Task) (List Trial)


type Tag
    = NoUnit
    | Critic
    | SpillOver


type alias TaggedSegment =
    ( Tag, String )


type Step
    = SPR (List TaggedSegment)
    | Feedback
    | Question


type Answer
    = Yes
    | No
    | Unsure


type alias State =
    { answer : Answer
    , step : Step
    , seenSegments : List { taggedSegment : TaggedSegment, startedAt : Int, endendAt : Int }
    }


initState segments =
    { answer = ""
    , step = SPR segments
    , seenSegments = []
    }


type alias Spr model =
    { model | spr : Logic.Task Trial State }


update : Msg -> Spr model -> ( Spr model, Cmd Msg )
update msg ({ spr } as model) =
    case msg of
        UserPressedSpaceToStartTraining timestamp ->
            Debug.todo ""

        UserPressedSpaceToReadNextSegment timestamp ->
            Debug.todo ""

        UserChoseNewAnswer newAnswer ->
            Debug.todo ""

        UserConfirmedChoice ->
            Debug.todo ""

        UserClickedNextTrial timestamp ->
            Debug.todo ""

        UserClickedSaveData ->
            Debug.todo ""

        ServerRespondedWithLastRecords (Result.Ok _) ->
            Debug.todo ""

        ServerRespondedWithLastRecords (Result.Err _) ->
            Debug.todo ""

        StartMain task trials ->
            Debug.todo ""

        RuntimeShuffledTrials infos trials ->
            let
                info =
                    Dict.fromList (List.map (\i -> ( i.uid, identity i )) infos)
                        |> Dict.get taskId 
                        |> Maybe.withDef
            in
            case trials of
                [] ->
                    ( model, Cmd.none )

                x :: y ->
                    ( { spr = Logic.startIntro info (List.filter (\trial -> trial.isTraining) trials) (List.filter (\trial -> not trial.isTraining) trials) initState }, Cmd.none )


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
