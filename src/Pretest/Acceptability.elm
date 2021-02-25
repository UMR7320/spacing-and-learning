module Pretest.Acceptability exposing (..)

import Array
import Data exposing (decodeRecords)
import Html.Styled exposing (Html, div, h3, p, text)
import Http
import Json.Decode as Decode
import Json.Decode.Pipeline exposing (..)
import Maybe
import Task
import Time


type Msg
    = UserClickedNextTrial



{--Acceptability ->
                    ( { model | route = Route.fromUrl url }
                    , fetchData (Route.fromUrl url)
                    )--}


view : Task -> { nextTrialMsg : msg } -> Html msg
view task { nextTrialMsg } =
    case task of
        NotStarted ->
            div [] [ text "Acceptability not started" ]

        Loading ->
            div [] [ text "Loading acceptability" ]

        DoingTask trials { evaluation } nTrial history ->
            let
                trial =
                    Array.get nTrial (Array.fromList trials) |> Maybe.withDefault defaultTrial
            in
            div []
                [ h3 [] [ text "Press Y if the sentence is correct, N if it's not. " ]
                , p [] [ text trial.sentence ]
                ]

        Failure reason ->
            div [] [ text "Oups I encounter an Issue while loading the acceptabiility task" ]

        Done ->
            div [] [ text "Acceptability task is done" ]


updateWhenNextTrial model timestampHandler nextTrialmsg =
    let
        state =
            getState model.acceptabilityTask
    in
    ( model
    , Task.perform (\time -> timestampHandler nextTrialmsg time) Time.now
    )


nextTrial : Time.Posix -> Task -> Task
nextTrial startedAt task =
    case task of
        DoingTask trials state ntrial history ->
            if ntrial >= List.length trials - 1 then
                Done

            else
                DoingTask trials { state | startedAt = Just startedAt } (ntrial + 1) history

        _ ->
            task


updateState : State -> Task -> Task
updateState newState task =
    case task of
        DoingTask trials state ntrial history ->
            DoingTask trials newState ntrial history

        _ ->
            task


getCurrentTrial : Task -> Trial
getCurrentTrial task =
    case task of
        DoingTask trials state ntrial history ->
            Array.get ntrial (Array.fromList trials) |> Maybe.withDefault defaultTrial

        _ ->
            defaultTrial


recordState : Task -> Task
recordState task =
    case task of
        DoingTask trials state ntrial history ->
            DoingTask trials state ntrial (state :: history)

        _ ->
            task


getState : Task -> State
getState task =
    case task of
        DoingTask trials state ntrial history ->
            state

        _ ->
            initState


decodeAcceptabilityTrials : Decode.Decoder (List Trial)
decodeAcceptabilityTrials =
    let
        decoder =
            Decode.succeed Trial
                |> required "UID" Decode.string
                |> required "Sentence" Decode.string
                |> optional "IsCorrect" Decode.bool False
                |> required "Duration" Decode.int
    in
    decodeRecords decoder


getTrialsFromServer : (Result Http.Error (List Trial) -> msg) -> Cmd msg
getTrialsFromServer callbackMsg =
    Data.getTrialsFromServer_ "acceptability" "all" callbackMsg decodeAcceptabilityTrials


defaultTrial : Trial
defaultTrial =
    { uid = "uidMISSING"
    , sentence = "sentenceMissig"
    , isCorrect = False
    , duration = 4000
    }


type alias Trial =
    { uid : String
    , sentence : String
    , isCorrect : Bool
    , duration : Int
    }


initState : State
initState =
    { useruid = "defaultUseruid"
    , trialuid = "defaulttrialuid"
    , evaluation = NoEvaluation
    , startedAt = Nothing
    , endedAt = Nothing
    }


type Task
    = NotStarted
    | Loading
    | DoingTask (List Trial) State CurrentTrialNumber History
    | Failure Http.Error
    | Done


type alias History =
    List State


type alias CurrentTrialNumber =
    Int


type alias State =
    { useruid : String
    , trialuid : String
    , evaluation : Evaluation
    , startedAt : Maybe Time.Posix
    , endedAt : Maybe Time.Posix
    }


type Evaluation
    = NoEvaluation
    | SentenceCorrect
    | SentenceIncorrect
    | EvaluationTimeOut
