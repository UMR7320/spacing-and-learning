module Session2.Session exposing (..)

import Debug exposing (todo)
import ExperimentInfo
import Http
import Random
import Random.Extra
import Random.List exposing (shuffle)
import Session
import Session2.CU2 as CU2
import Session2.Spelling as Scrabble
import Session2.Translation as Translation
import Task.Parallel as Para


type alias Session2 =
    Session.Session (Para.State4 Msg (List CU2.Trial) (List Scrabble.Trial) (List Translation.Trial) (List ExperimentInfo.Task))


type Msg
    = ServerRespondedWithSomeData (Para.Msg4 (List CU2.Trial) (List Scrabble.Trial) (List Translation.Trial) (List ExperimentInfo.Task))
    | ServerRespondedWithAllData (List CU2.Trial) (List Scrabble.Trial) (List Translation.Trial) (List ExperimentInfo.Task)
    | ServerRespondedWithSomeError Http.Error
    | StartSession ShuffledSession2


type alias ShuffledSession2 =
    { cu : List CU2.Trial, spelling : List Scrabble.Trial, translation : List Translation.Trial, infos : List ExperimentInfo.Task }


getAll =
    Para.attempt4
        { task1 = CU2.getRecords
        , task2 = Scrabble.getRecords
        , task3 = Translation.getRecords
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
                                Random.map5 Scrabble.Trial
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

        ServerRespondedWithSomeError _ ->
            todo ""

        StartSession { cu, spelling, translation, infos } ->
            ( { model
                | translationTask = Translation.start infos translation
                , cuLvl2 = CU2.start infos cu
                , scrabbleTask = Scrabble.start infos spelling
                , session2 = Session.Ready
              }
            , Cmd.none
            )
