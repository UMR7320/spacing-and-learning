module Session1.Session exposing (..)

import Activity
import ActivityInfo exposing (ActivityInfo)
import Data
import Http
import Random
import Random.List exposing (shuffle)
import Session exposing (Session(..))
import Session1.Context1 as Context1 exposing (Context1)
import Session1.Spelling1 as Spelling1 exposing (Spelling1)
import Task.Parallel as Para


type alias Session1 =
    Session (Para.State3 Msg (List Spelling1.Trial) (List Context1.Trial) (List ActivityInfo))


type alias Model superModel =
    { superModel
        | session1 : Session1
        , spelling1 : Spelling1
        , context1 : Context1
    }


type alias ShuffledSession1 =
    { spelling1 : List Spelling1.Trial
    , context1 : List Context1.Trial
    , infos : List ActivityInfo
    }


type Msg
    = ServerRespondedWithSomeData LoadingMsg
    | ServerRespondedWithSomeError Http.Error
    | ServerRespondedWithAllData (List Spelling1.Trial) (List Context1.Trial) (List ActivityInfo)
    | StartSession ShuffledSession1


type alias LoadingMsg =
    Para.Msg3 (List Spelling1.Trial) (List Context1.Trial) (List ActivityInfo)


getAll =
    Para.attempt3
        { task1 = Spelling1.getRecords
        , task2 = Context1.getRecords
        , task3 = ActivityInfo.getRecords
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
                            Para.update3 downloadState downloadMsg |> Tuple.mapFirst Loading

                        _ ->
                            ( model.session1, Cmd.none )
            in
            ( { model | session1 = updte }, cmd )

        ServerRespondedWithSomeError error ->
            ( { model
                | context1 = Activity.Err (Data.buildErrorMessage error)
                , spelling1 = Activity.Err (Data.buildErrorMessage error)
              }
            , Cmd.none
            )

        ServerRespondedWithAllData spelling1 context1 infos ->
            let
                randomize =
                    Random.generate StartSession (Random.map3 ShuffledSession1 (shuffle spelling1) (shuffle context1) (Random.constant infos))
            in
            ( model
            , randomize
            )

        StartSession ({ infos } as tasks) ->
            ( { model
                | session1 = Ready
                , spelling1 = Spelling1.start infos tasks.spelling1
                , context1 = Context1.start infos tasks.context1
              }
            , Cmd.none
            )
