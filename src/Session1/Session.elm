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
import Session1.ContextUnderstanding as CU
import Session1.Meaning as Meaning
import Session1.Presentation as Presentation
import Session1.Spelling as Spelling
import Task.Parallel as Para


type alias Session1 =
    Session (Para.State5 Msg (List Meaning.Trial) (List Spelling.Trial) (List CU.Trial) (List Presentation.Trial) (List ExperimentInfo.Activity))


type alias Model superModel =
    { superModel
        | meaning : Meaning.Meaning
        , spellingLvl1 : Spelling.Spelling
        , cu1 : CU.CU
        , presentation : Presentation.Presentation
        , infos : RemoteData.RemoteData Http.Error (Dict.Dict String ExperimentInfo.Activity)
        , session1 : Session1
    }


type alias ShuffledSession1 =
    { meaning : List Meaning.Trial
    , spelling : List Spelling.Trial
    , cu1 : List CU.Trial
    , presentation : List Presentation.Trial
    , infos_ : List ExperimentInfo.Activity
    }


type Msg
    = ServerRespondedWithSomeData LoadingMsg
    | ServerRespondedWithSomeError Http.Error
    | ServerRespondedWithAllData (List Meaning.Trial) (List Spelling.Trial) (List CU.Trial) (List Presentation.Trial) (List ExperimentInfo.Activity)
    | StartSession ShuffledSession1


type alias LoadingMsg =
    Para.Msg5 (List Meaning.Trial) (List Spelling.Trial) (List CU.Trial) (List Presentation.Trial) (List ExperimentInfo.Activity)


getAll =
    Para.attempt5
        { task1 = Meaning.getRecords
        , task2 = Spelling.getRecords
        , task3 = CU.getRecords
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
                | meaning = Logic.Err (Data.buildErrorMessage error)
                , cu1 = Logic.Err (Data.buildErrorMessage error)
                , spellingLvl1 = Logic.Err (Data.buildErrorMessage error)
              }
            , Cmd.none
            )

        ServerRespondedWithAllData meaning spelling cu1 presentation infos_ ->
            let
                randomize =
                    Random.generate StartSession (Random.map5 ShuffledSession1 (shuffle meaning) (shuffle spelling) (shuffle cu1) (shuffle presentation) (Random.constant infos_))
            in
            ( model
            , randomize
            )

        StartSession ({ infos_ } as tasks) ->
            ( { model
                | meaning = Meaning.start infos_ tasks.meaning
                , spellingLvl1 = Spelling.start infos_ tasks.spelling
                , cu1 = CU.start infos_ tasks.cu1
                , presentation = Presentation.start infos_ tasks.presentation
                , session1 = Ready
              }
            , Cmd.none
            )
