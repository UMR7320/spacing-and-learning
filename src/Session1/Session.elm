module Session1.Session exposing (..)

import Activity
import ActivityInfo exposing (ActivityInfo)
import Data
import Http
import Random
import Random.List exposing (shuffle)
import Session exposing (Session(..))
import Session1.Context1 as Context1 exposing (Context1)
import Task.Parallel as Para


type alias Session1 =
    Session (Para.State2 Msg (List Context1.Trial) (List ActivityInfo))


type alias Model superModel =
    { superModel
        | session1 : Session1
        , context1 : Context1
    }


type alias ShuffledSession1 =
    { context1 : List Context1.Trial
    , infos : List ActivityInfo
    }


type Msg
    = ServerRespondedWithSomeData LoadingMsg
    | ServerRespondedWithSomeError Http.Error
    | ServerRespondedWithAllData (List Context1.Trial) (List ActivityInfo)
    | StartSession ShuffledSession1


type alias LoadingMsg =
    Para.Msg2 (List Context1.Trial) (List ActivityInfo)


getAll =
    Para.attempt2
        { task1 = Context1.getRecords
        , task2 = ActivityInfo.getRecords
        , onUpdates = ServerRespondedWithSomeData
        , onFailure = ServerRespondedWithSomeError
        , onSuccess = ServerRespondedWithAllData
        }


update : Msg -> Model superModel -> ( Model superModel, Cmd Msg )
update msg model =
    case msg of
        ServerRespondedWithSomeData downloadMsg ->
            let
                ( updte, cmd ) =
                    case model.session1 of
                        Loading downloadState ->
                            Para.update2 downloadState downloadMsg |> Tuple.mapFirst Loading

                        _ ->
                            ( model.session1, Cmd.none )
            in
            ( { model | session1 = updte }, cmd )

        ServerRespondedWithSomeError error ->
            ( { model
                | context1 = Activity.Err (Data.buildErrorMessage error)
              }
            , Cmd.none
            )

        ServerRespondedWithAllData context1 infos ->
            let
                randomize =
                    Random.generate StartSession (Random.map2 ShuffledSession1 (shuffle context1) (Random.constant infos))
            in
            ( model
            , randomize
            )

        StartSession ({ infos } as tasks) ->
            ( { model
                | session1 = Ready
                , context1 = Context1.start infos tasks.context1
              }
            , Cmd.none
            )
