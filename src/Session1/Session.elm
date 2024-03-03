module Session1.Session exposing (..)

import Activity
import ActivityInfo exposing (ActivityInfo)
import Data
import Http
import Random
import Random.List exposing (shuffle)
import Session exposing (Session(..))
import Session1.Context1 as Context1 exposing (Context1)
import Session1.Meaning1 as Meaning1 exposing (Meaning1)
import Session1.Spelling1 as Spelling1 exposing (Spelling1)
import Task.Parallel as Para


type alias Session1 =
    Session (Para.State4 Msg (List Meaning1.Trial) (List Spelling1.Trial) (List Context1.Trial) (List ActivityInfo))


type alias Model superModel =
    { superModel
        | session1 : Session1
        , meaning1 : Meaning1
        , spelling1 : Spelling1
        , context1 : Context1
    }


type alias ShuffledSession1 =
    { meaning1 : List Meaning1.Trial
    , spelling1 : List Spelling1.Trial
    , context1 : List Context1.Trial
    , infos : List ActivityInfo
    }


type Msg
    = ServerRespondedWithSomeData LoadingMsg
    | ServerRespondedWithSomeError Http.Error
    | ServerRespondedWithAllData (List Meaning1.Trial) (List Spelling1.Trial) (List Context1.Trial) (List ActivityInfo)
    | StartSession ShuffledSession1


type alias LoadingMsg =
    Para.Msg4 (List Meaning1.Trial) (List Spelling1.Trial) (List Context1.Trial) (List ActivityInfo)


getAll =
    Para.attempt4
        { task1 = Meaning1.getRecords
        , task2 = Spelling1.getRecords
        , task3 = Context1.getRecords
        , task4 = ActivityInfo.getRecords
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
                            Para.update4 downloadState downloadMsg |> Tuple.mapFirst Loading

                        _ ->
                            ( model.session1, Cmd.none )
            in
            ( { model | session1 = updte }, cmd )

        ServerRespondedWithSomeError error ->
            ( { model
                | meaning1 = Activity.Err (Data.buildErrorMessage error)
                , context1 = Activity.Err (Data.buildErrorMessage error)
                , spelling1 = Activity.Err (Data.buildErrorMessage error)
              }
            , Cmd.none
            )

        ServerRespondedWithAllData meaning1 spelling1 context1 infos ->
            let
                randomize =
                    Random.generate StartSession (Random.map4 ShuffledSession1 (shuffle meaning1) (shuffle spelling1) (shuffle context1) (Random.constant infos))
            in
            ( model
            , randomize
            )

        StartSession ({ infos } as tasks) ->
            ( { model
                | session1 = Ready
                , meaning1 = Meaning1.start infos tasks.meaning1
                , spelling1 = Spelling1.start infos tasks.spelling1
                , context1 = Context1.start infos tasks.context1
              }
            , Cmd.none
            )
