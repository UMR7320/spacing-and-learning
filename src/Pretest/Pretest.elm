module Pretest.Pretest exposing (..)

import ExperimentInfo
import Http
import Pretest.SPR as SPR
import Pretest.SentenceCompletion as SentenceCompletion
import Task.Parallel as Para


type alias Pretest =
    Para.State3 Msg (List SPR.Trial) (List SentenceCompletion.Trial) (List ExperimentInfo.Task)


type alias ParaMsg =
    Para.Msg3 (List SPR.Trial) (List SentenceCompletion.Trial) (List ExperimentInfo.Task)


attempt : ( Pretest, Cmd Msg )
attempt =
    Para.attempt3
        { task1 = SPR.getRecords
        , task2 = SentenceCompletion.getRecords
        , task3 = ExperimentInfo.getRecords
        , onUpdates = ServerRespondedWithSomePretestData
        , onFailure = ServerRespondedWithSomeError
        , onSuccess = ServerRespondedWithAllPretestData
        }


type Msg
    = ServerRespondedWithSomePretestData ParaMsg
    | ServerRespondedWithSomeError Http.Error
    | ServerRespondedWithAllPretestData (List SPR.Trial) (List SentenceCompletion.Trial) (List ExperimentInfo.Task)
