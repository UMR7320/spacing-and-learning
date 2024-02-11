module Session1.Session exposing (..)

import Data
import Dict
import ExperimentInfo
import Http
import Logic
import Random
import Random.List exposing (shuffle)
import RemoteData
import Session exposing (Session(..))
import Session1.Context1 as Context1 exposing (Context1)
import Session1.Meaning1 as Meaning1 exposing (Meaning1)
import Session1.Presentation as Presentation
import Session1.Spelling1 as Spelling1 exposing (Spelling1)
import Task.Parallel as Para


type alias Session1 =
    Session (Para.State5 Msg (List Meaning1.Trial) (List Spelling1.Trial) (List Context1.Trial) (List Presentation.Trial) (List ExperimentInfo.Activity))


type alias Model superModel =
    { superModel
        | session1 : Session1
        , presentation : Presentation.Presentation
        , meaning1 : Meaning1
        , spelling1 : Spelling1
        , context1 : Context1
        , infos : RemoteData.RemoteData Http.Error (Dict.Dict String ExperimentInfo.Activity)
    }


type alias ShuffledSession1 =
    { meaning1 : List Meaning1.Trial
    , spelling1 : List Spelling1.Trial
    , context1 : List Context1.Trial
    , presentation : List Presentation.Trial
    , infos_ : List ExperimentInfo.Activity
    }


type Msg
    = ServerRespondedWithSomeData LoadingMsg
    | ServerRespondedWithSomeError Http.Error
    | ServerRespondedWithAllData (List Meaning1.Trial) (List Spelling1.Trial) (List Context1.Trial) (List Presentation.Trial) (List ExperimentInfo.Activity)
    | StartSession ShuffledSession1


type alias LoadingMsg =
    Para.Msg5 (List Meaning1.Trial) (List Spelling1.Trial) (List Context1.Trial) (List Presentation.Trial) (List ExperimentInfo.Activity)


getAll =
    Para.attempt5
        { task1 = Meaning1.getRecords
        , task2 = Spelling1.getRecords
        , task3 = Context1.getRecords
        , task4 = Presentation.getRecords
        , task5 = ExperimentInfo.getRecords
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
                            Para.update5 downloadState downloadMsg |> Tuple.mapFirst Loading

                        _ ->
                            ( model.session1, Cmd.none )
            in
            ( { model | session1 = updte }, cmd )

        ServerRespondedWithSomeError error ->
            ( { model
                | meaning1 = Logic.Err (Data.buildErrorMessage error)
                , context1 = Logic.Err (Data.buildErrorMessage error)
                , spelling1 = Logic.Err (Data.buildErrorMessage error)
              }
            , Cmd.none
            )

        ServerRespondedWithAllData meaning1 spelling1 context1 presentation infos_ ->
            let
                randomize =
                    Random.generate StartSession (Random.map5 ShuffledSession1 (shuffle meaning1) (shuffle spelling1) (shuffle context1) (shuffle presentation) (Random.constant infos_))
            in
            ( model
            , randomize
            )

        StartSession ({ infos_ } as tasks) ->
            ( { model
                | session1 = Ready
                , presentation = Presentation.start infos_ tasks.presentation
                , meaning1 = Meaning1.start infos_ tasks.meaning1
                , spelling1 = Spelling1.start infos_ tasks.spelling1
                , context1 = Context1.start infos_ tasks.context1
              }
            , Cmd.none
            )
