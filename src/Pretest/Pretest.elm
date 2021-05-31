module Pretest.Pretest exposing (..)

import Data
import Delay
import ExperimentInfo
import Html.Attributes exposing (accept)
import Http
import List.Extra
import Logic
import Pretest.Acceptability as Acceptability exposing (ErrorBlock)
import Pretest.SPR as SPR
import Pretest.SentenceCompletion as SentenceCompletion
import Pretest.VKS as VKS
import Random
import Random.Extra
import Random.List exposing (shuffle)
import Task.Parallel as Para


type alias Pretest =
    Para.State5 Msg (List SPR.Trial) (List SentenceCompletion.Trial) (List ExperimentInfo.Task) (List VKS.Trial) (List Acceptability.Trial)


type alias ParaMsg =
    Para.Msg5 (List SPR.Trial) (List SentenceCompletion.Trial) (List ExperimentInfo.Task) (List VKS.Trial) (List Acceptability.Trial)


attempt : ( Pretest, Cmd Msg )
attempt =
    Para.attempt5
        { task1 = SPR.getRecords
        , task2 = SentenceCompletion.getRecords
        , task3 = ExperimentInfo.getRecords
        , task4 = VKS.getRecords
        , task5 = Acceptability.getRecords
        , onUpdates = ServerRespondedWithSomePretestData
        , onFailure = ServerRespondedWithSomeError
        , onSuccess = ServerRespondedWithAllPretestData
        }


type Msg
    = ServerRespondedWithSomePretestData ParaMsg
    | ServerRespondedWithSomeError Http.Error
    | ServerRespondedWithAllPretestData (List SPR.Trial) (List SentenceCompletion.Trial) (List ExperimentInfo.Task) (List VKS.Trial) (List Acceptability.Trial)
    | StartPretest ShuffledPretest


type alias ShuffledPretest =
    { spr : List SPR.Trial
    , sc : List SentenceCompletion.Trial
    , infos : List ExperimentInfo.Task
    , vks : List VKS.Trial
    , acceptability :
        Result
            ( ErrorBlock, List Acceptability.Trial )
            (List (List Acceptability.Trial))
    }


type alias Model superModel =
    { superModel | spr : SPR.SPR, sentenceCompletion : SentenceCompletion.SentenceCompletion, pretest : Pretest, vks : Logic.Task VKS.Trial VKS.State, acceptabilityTask : Logic.Task Acceptability.Trial Acceptability.State }


update : Msg -> Model superModel -> ( Model superModel, Cmd Msg )
update msg model =
    case msg of
        ServerRespondedWithSomePretestData downloaded ->
            let
                ( nextState, nextCmd ) =
                    Para.update5 model.pretest downloaded
            in
            ( { model | pretest = nextState }, nextCmd )

        ServerRespondedWithSomeError err ->
            ( { model
                | spr = Logic.Err (Data.buildErrorMessage err)
                , sentenceCompletion = Logic.Err (Data.buildErrorMessage err)
                , vks = Logic.Err (Data.buildErrorMessage err)
                , acceptabilityTask = Logic.Err (Data.buildErrorMessage err)
              }
            , Cmd.none
            )

        ServerRespondedWithAllPretestData sprtrials sctrials infos vksTrials aTrials ->
            let
                randomize =
                    Random.generate StartPretest (Random.map5 ShuffledPretest (shuffle sprtrials) (shuffle sctrials) (Random.constant infos) (shuffle vksTrials) generateOrganizedTrials)

                generateOrganizedTrials =
                    Random.List.shuffle aTrials
                        |> Random.andThen
                            (\shuffledTrials ->
                                let
                                    targets =
                                        List.filter (\datum -> datum.trialType == Acceptability.Target) shuffledTrials

                                    distractors =
                                        List.filter (\datum -> datum.trialType == Acceptability.Distractor) shuffledTrials
                                in
                                Random.constant (Acceptability.organizeAcceptabilityTrials targets distractors)
                                    |> Random.andThen
                                        (\organizedTrials_ ->
                                            case organizedTrials_ of
                                                Result.Err reason ->
                                                    Random.constant (Result.Err reason)

                                                Result.Ok tr ->
                                                    Random.Extra.sequence (swapTargetWithOneDistractor tr)
                                                        |> Random.andThen
                                                            (\swapedTargets ->
                                                                Random.constant
                                                                    (Result.Ok
                                                                        (trainingTrials :: swapedTargets)
                                                                    )
                                                            )
                                        )
                            )

                trainingTrials =
                    List.filter (\datum -> datum.trialType == Acceptability.Training) aTrials

                swapTargetWithOneDistractor : List (List Acceptability.Trial) -> List (Random.Generator (List Acceptability.Trial))
                swapTargetWithOneDistractor tr =
                    List.map
                        (\block ->
                            Random.int 1 (List.length block - 1)
                                |> Random.andThen
                                    (\position ->
                                        Random.constant (List.Extra.swapAt 0 position block)
                                    )
                        )
                        tr
            in
            ( model, randomize )

        StartPretest { spr, sc, vks, infos, acceptability } ->
            case acceptability of
                Result.Ok shuffledTrials ->
                    if List.filter (\block -> List.length block < 4) shuffledTrials == [ [] ] then
                        ( { model
                            | acceptabilityTask = Acceptability.start infos (List.concat shuffledTrials)
                            , spr = SPR.init infos spr
                            , sentenceCompletion = SentenceCompletion.init infos sc
                            , vks = VKS.init infos vks
                          }
                        , Cmd.none
                        )

                    else
                        update (ServerRespondedWithAllPretestData spr sc infos vks (List.concat shuffledTrials)) model

                Result.Err reason ->
                    ( { model | acceptabilityTask = Logic.Err "Error trying to build blocks" }, Cmd.none )
