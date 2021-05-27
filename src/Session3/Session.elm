module Session3.Session exposing (..)

import Data
import Debug exposing (todo)
import ExperimentInfo
import Http
import Logic
import Random
import Random.List exposing (shuffle)
import RemoteData
import Session
import Session2.Session exposing (Msg(..))
import Session3.CU3 as CU
import Session3.Spelling3 as Spelling
import Session3.Synonym as Synonym
import Task.Parallel as Para


attempt =
    Para.attempt4
        { task1 = CU.getRecords
        , task2 = Spelling.getRecords
        , task3 = Synonym.getRecords
        , task4 = ExperimentInfo.getRecords
        , onUpdates = ServerRespondedWithSomeSession3Data
        , onFailure = ServerRespondedWithSomeError
        , onSuccess = ServerRespondedWithAllSession3Data
        }


type alias ShuffledSession3 =
    { cu : List CU.Trial, spelling : List Spelling.Trial, synonym : List Synonym.Trial, infos : List ExperimentInfo.Task }


type Msg
    = ServerRespondedWithSomeSession3Data (Para.Msg4 (List CU.Trial) (List Spelling.Trial) (List Synonym.Trial) (List ExperimentInfo.Task))
    | ServerRespondedWithAllSession3Data (List CU.Trial) (List Spelling.Trial) (List Synonym.Trial) (List ExperimentInfo.Task)
    | ServerRespondedWithSomeError Http.Error
    | StartSession ShuffledSession3


type alias Session3 =
    Session.Session (Para.State4 Msg (List CU.Trial) (List Spelling.Trial) (List Synonym.Trial) (List ExperimentInfo.Task))


update msg model =
    case msg of
        ServerRespondedWithSomeSession3Data downloadMsg ->
            let
                ( updte, cmd ) =
                    case model.session3 of
                        Session.Loading downloadState ->
                            Para.update4 downloadState downloadMsg |> Tuple.mapFirst Session.Loading

                        _ ->
                            ( model.session3, Cmd.none )
            in
            ( { model | session3 = updte }, cmd )

        ServerRespondedWithAllSession3Data cu spelling synonym infos ->
            let
                randomize =
                    Random.generate StartSession (Random.map4 ShuffledSession3 (shuffle cu) (shuffle spelling) (shuffle synonym) (Random.constant infos))
            in
            ( model
            , randomize
            )

        ServerRespondedWithSomeError error ->
            ( { model
                | cu3 = Logic.Err (Data.buildErrorMessage error)
                , spelling3 = Logic.Err (Data.buildErrorMessage error)
                , synonymTask = Logic.Err (Data.buildErrorMessage error)
              }
            , Cmd.none
            )

        StartSession { cu, spelling, synonym, infos } ->
            ( { model
                | synonymTask = Synonym.start infos synonym
                , spelling3 = Spelling.start infos spelling
                , cu3 = CU.start infos cu
                , session3 = Session.Ready
                , infos = RemoteData.Success (ExperimentInfo.toDict infos)
              }
            , Cmd.none
            )

