module Session2.Session exposing (..)

import Data
import ExperimentInfo
import Http
import Activity
import Random
import Random.Extra
import Random.List exposing (shuffle)
import Session
import Session2.Context2 as Context2
import Session2.Meaning2 as Meaning2
import Session2.Spelling2 as Spelling2
import Task.Parallel as Para


type alias Session2 =
    Session.Session (Para.State4 Msg (List Context2.Trial) (List Spelling2.Trial) (List Meaning2.Trial) (List ExperimentInfo.Activity))


type Msg
    = ServerRespondedWithSomeData (Para.Msg4 (List Context2.Trial) (List Spelling2.Trial) (List Meaning2.Trial) (List ExperimentInfo.Activity))
    | ServerRespondedWithAllData (List Context2.Trial) (List Spelling2.Trial) (List Meaning2.Trial) (List ExperimentInfo.Activity)
    | ServerRespondedWithSomeError Http.Error
    | StartSession ShuffledSession2


type alias ShuffledSession2 =
    { cu : List Context2.Trial, spelling : List Spelling2.Trial, translation : List Meaning2.Trial, infos : List ExperimentInfo.Activity }


getAll =
    Para.attempt4
        { task1 = Context2.getRecords
        , task2 = Spelling2.getRecords
        , task3 = Meaning2.getRecords
        , task4 = ExperimentInfo.getRecords
        , onUpdates = ServerRespondedWithSomeData
        , onFailure = ServerRespondedWithSomeError
        , onSuccess = ServerRespondedWithAllData
        }


update msg model =
    case msg of
        ServerRespondedWithSomeData dataSoFar ->
            let
                ( updte, cmd ) =
                    case model.session2 of
                        Session.Loading downloadState ->
                            Para.update4 downloadState dataSoFar |> Tuple.mapFirst Session.Loading

                        _ ->
                            ( model.session2, Cmd.none )
            in
            ( { model | session2 = updte }, cmd )

        ServerRespondedWithAllData cu spelling translation infos_ ->
            let
                shuffleLetters =
                    --shuffleLetters recreates each trial in shuffling every each word's letter. When it's done it shuffles trials.
                    spelling
                        |> List.map
                            (\trial ->
                                Random.map5 Spelling2.Trial
                                    (Random.constant trial.uid)
                                    (trial.writtenWord
                                        |> String.toList
                                        |> shuffle
                                        |> Random.map
                                            (\letters_ ->
                                                letters_
                                                    |> List.map String.fromChar
                                                    |> String.concat
                                            )
                                    )
                                    (Random.constant trial.audioWord)
                                    (Random.constant trial.isTraining)
                                    (Random.constant trial.writtenWord)
                            )
                        |> Random.Extra.sequence
                        |> Random.andThen Random.List.shuffle

                randomizeTrials =
                    Random.generate StartSession (Random.map4 ShuffledSession2 (shuffle cu) shuffleLetters (shuffle translation) (Random.constant infos_))
            in
            ( model
            , Cmd.batch [ randomizeTrials ]
            )

        ServerRespondedWithSomeError reason ->
            ( { model
                | meaning2 = Activity.Err (Data.buildErrorMessage reason)
                , context2 = Activity.Err (Data.buildErrorMessage reason)
                , spelling2 = Activity.Err (Data.buildErrorMessage reason)
                , session2 = Session.Error (Data.buildErrorMessage reason)
              }
            , Cmd.none
            )

        StartSession { cu, spelling, translation, infos } ->
            ( { model
                | meaning2 = Meaning2.start infos translation
                , context2 = Context2.start infos cu
                , spelling2 = Spelling2.start infos spelling
                , session2 = Session.Ready
              }
            , Cmd.none
            )
