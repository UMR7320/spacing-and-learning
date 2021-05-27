module Pretest.Pretest exposing (..)

import Data
import ExperimentInfo
import Http
import Logic
import Pretest.SPR as SPR
import Pretest.SentenceCompletion as SentenceCompletion
import Pretest.VKS as VKS
import Random
import Random.List exposing (shuffle)
import Task.Parallel as Para


type alias Pretest =
    Para.State4 Msg (List SPR.Trial) (List SentenceCompletion.Trial) (List ExperimentInfo.Task) (List VKS.Trial)


type alias ParaMsg =
    Para.Msg4 (List SPR.Trial) (List SentenceCompletion.Trial) (List ExperimentInfo.Task) (List VKS.Trial)


attempt : ( Pretest, Cmd Msg )
attempt =
    Para.attempt4
        { task1 = SPR.getRecords
        , task2 = SentenceCompletion.getRecords
        , task3 = ExperimentInfo.getRecords
        , task4 = VKS.getRecords
        , onUpdates = ServerRespondedWithSomePretestData
        , onFailure = ServerRespondedWithSomeError
        , onSuccess = ServerRespondedWithAllPretestData
        }


type Msg
    = ServerRespondedWithSomePretestData ParaMsg
    | ServerRespondedWithSomeError Http.Error
    | ServerRespondedWithAllPretestData (List SPR.Trial) (List SentenceCompletion.Trial) (List ExperimentInfo.Task) (List VKS.Trial)
    | StartPretest ShuffledPretest


type alias ShuffledPretest =
    { spr : List SPR.Trial, sc : List SentenceCompletion.Trial, infos : List ExperimentInfo.Task, vks : List VKS.Trial }


type alias Model superModel =
    { superModel | spr : SPR.SPR, sentenceCompletion : SentenceCompletion.SentenceCompletion, pretest : Pretest, vks : Logic.Task VKS.Trial VKS.State }


update : Msg -> Model superModel -> ( Model superModel, Cmd Msg )
update msg model =
    case msg of
        ServerRespondedWithSomePretestData downloaded ->
            let
                ( nextState, nextCmd ) =
                    Para.update4 model.pretest downloaded
            in
            ( { model | pretest = nextState }, nextCmd )

        ServerRespondedWithSomeError err ->
            ( { model
                | spr = Logic.Err (Data.buildErrorMessage err)
                , sentenceCompletion = Logic.Err (Data.buildErrorMessage err)
                , vks = Logic.Err (Data.buildErrorMessage err)
              }
            , Cmd.none
            )

        ServerRespondedWithAllPretestData sprtrials sctrials infos vksTrials ->
            let
                randomize =
                    Random.generate StartPretest (Random.map4 ShuffledPretest (shuffle sprtrials) (shuffle sctrials) (Random.constant infos) (shuffle vksTrials))
            in
            ( model, randomize )

        StartPretest { spr, sc, vks, infos } ->
            ( { model
                | spr = SPR.init infos spr
                , sentenceCompletion = SentenceCompletion.init infos sc
                , vks = VKS.init infos vks
              }
            , Cmd.none
            )
