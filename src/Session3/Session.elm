module Session3.Session exposing (..)

import Data
import ExperimentInfo
import Http
import Activity
import Random
import Random.List exposing (shuffle)
import RemoteData
import Session
import Session2.Session exposing (Msg(..))
import Session3.Context3 as Context3
import Session3.Meaning3 as Meaning3
import Session3.Spelling3 as Spelling3
import Task.Parallel as Para


attempt =
    Para.attempt4
        { task1 = Context3.getRecords
        , task2 = Spelling3.getRecords
        , task3 = Meaning3.getRecords
        , task4 = ExperimentInfo.getRecords
        , onUpdates = ServerRespondedWithSomeSession3Data
        , onFailure = ServerRespondedWithSomeError
        , onSuccess = ServerRespondedWithAllSession3Data
        }


type alias ShuffledSession3 =
    { context3 : List Context3.Trial
    , spelling3 : List Spelling3.Trial
    , meaning3 : List Meaning3.Trial
    , infos : List ExperimentInfo.Activity
    }


type Msg
    = ServerRespondedWithSomeSession3Data (Para.Msg4 (List Context3.Trial) (List Spelling3.Trial) (List Meaning3.Trial) (List ExperimentInfo.Activity))
    | ServerRespondedWithAllSession3Data (List Context3.Trial) (List Spelling3.Trial) (List Meaning3.Trial) (List ExperimentInfo.Activity)
    | ServerRespondedWithSomeError Http.Error
    | StartSession ShuffledSession3


type alias Session3 =
    Session.Session (Para.State4 Msg (List Context3.Trial) (List Spelling3.Trial) (List Meaning3.Trial) (List ExperimentInfo.Activity))


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

        ServerRespondedWithAllSession3Data context3 spelling3 meaning3 infos ->
            let
                randomize =
                    Random.generate StartSession (Random.map4 ShuffledSession3 (shuffle context3) (shuffle spelling3) (shuffle meaning3) (Random.constant infos))
            in
            ( model
            , randomize
            )

        ServerRespondedWithSomeError error ->
            ( { model
                | context3 = Activity.Err (Data.buildErrorMessage error)
                , spelling3 = Activity.Err (Data.buildErrorMessage error)
                , meaning3 = Activity.Err (Data.buildErrorMessage error)
              }
            , Cmd.none
            )

        StartSession { context3, spelling3, meaning3, infos } ->
            ( { model
                | meaning3 = Meaning3.start infos meaning3
                , spelling3 = Spelling3.start infos spelling3
                , context3 = Context3.start infos context3
                , session3 = Session.Ready
                , infos = RemoteData.Success (ExperimentInfo.toDict infos)
              }
            , Cmd.none
            )
