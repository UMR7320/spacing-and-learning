module Pretest.Pretest exposing (..)

import Activity
import ActivityInfo exposing (ActivityInfo)
import Data
import Http
import List.Extra
import Pretest.Acceptability as Acceptability exposing (Acceptability, ErrorBlock)
import Pretest.SPR as SPR
import Pretest.SentenceCompletion as SentenceCompletion exposing (SentenceCompletion)
import Pretest.VKS as VKS exposing (VKS)
import Pretest.Version exposing (Version)
import Random
import Random.Extra
import Random.List exposing (shuffle)
import Session
import Task.Parallel as Para



-- MODEL


type alias Pretest =
    Session.Session (Para.State5 Msg (List SPR.Trial) (List SentenceCompletion.Trial) (List ActivityInfo) (List VKS.Trial) (List Acceptability.Trial))


type alias ParaMsg =
    Para.Msg5 (List SPR.Trial) (List SentenceCompletion.Trial) (List ActivityInfo) (List VKS.Trial) (List Acceptability.Trial)


type alias ShuffledPretest =
    { spr : List SPR.Trial
    , sc : List SentenceCompletion.Trial
    , infos : List ActivityInfo
    , vks : List VKS.Trial
    , acceptability :
        Result ( ErrorBlock, List Acceptability.Trial ) (List (List Acceptability.Trial))
    }


type alias Model superModel =
    { superModel
        | spr : SPR.SPR
        , sentenceCompletion : SentenceCompletion
        , pretest : Pretest
        , vks : { task : VKS, showVideo : Bool }
        , acceptability : Acceptability
        , version : Version
        , user : Maybe String
    }



-- UPDATE


type Msg
    = ServerRespondedWithSomePretestData ParaMsg
    | ServerRespondedWithSomeError Http.Error
    | ServerRespondedWithAllPretestData (List SPR.Trial) (List SentenceCompletion.Trial) (List ActivityInfo) (List VKS.Trial) (List Acceptability.Trial)
    | StartPretest ShuffledPretest


update : Msg -> Model superModel -> ( Model superModel, Cmd Msg )
update msg model =
    case msg of
        ServerRespondedWithSomePretestData downloaded ->
            let
                ( updte, cmd ) =
                    case model.pretest of
                        Session.Loading downloadState ->
                            Para.update5 downloadState downloaded |> Tuple.mapFirst Session.Loading

                        _ ->
                            ( model.pretest, Cmd.none )
            in
            ( { model | pretest = updte }, cmd )

        ServerRespondedWithSomeError err ->
            let
                vks =
                    model.vks

                updatedVks =
                    { vks | task = Activity.Err (Data.buildErrorMessage err) }
            in
            ( { model
                | spr = Activity.Err (Data.buildErrorMessage err)
                , sentenceCompletion = Activity.Err (Data.buildErrorMessage err)
                , vks = updatedVks
                , acceptability = Activity.Err (Data.buildErrorMessage err)
              }
            , Cmd.none
            )

        ServerRespondedWithAllPretestData sprtrials sctrials infos vksTrials aTrials ->
            let
                randomize =
                    Random.generate StartPretest (Random.map5 ShuffledPretest (shuffle sprtrials) (shuffle sctrials) (Random.constant infos) (shuffle vksTrials) generateOrganizedTrials)

                generateOrganizedTrials =
                    Random.List.shuffle aTrials
                        |> Random.andThen
                            (\shuffledTrials ->
                                let
                                    targets =
                                        List.filter (\datum -> datum.trialType == Acceptability.Target) shuffledTrials

                                    distractors =
                                        List.filter (\datum -> datum.trialType == Acceptability.Distractor) shuffledTrials
                                in
                                Random.constant (Acceptability.organizeAcceptabilityTrials targets distractors)
                                    |> Random.andThen
                                        (\organizedTrials_ ->
                                            case organizedTrials_ of
                                                Result.Err reason ->
                                                    Random.constant (Result.Err reason)

                                                Result.Ok tr ->
                                                    Random.Extra.sequence (swapTargetWithOneDistractor tr)
                                                        |> Random.andThen
                                                            (\swapedTargets ->
                                                                Random.constant
                                                                    (Result.Ok
                                                                        (trainingTrials :: swapedTargets)
                                                                    )
                                                            )
                                        )
                            )

                trainingTrials =
                    List.filter (\datum -> datum.trialType == Acceptability.Training) aTrials

                swapTargetWithOneDistractor : List (List Acceptability.Trial) -> List (Random.Generator (List Acceptability.Trial))
                swapTargetWithOneDistractor tr =
                    List.map
                        (\block ->
                            Random.int 1 (List.length block - 1)
                                |> Random.andThen
                                    (\position ->
                                        Random.constant (List.Extra.swapAt 0 position block)
                                    )
                        )
                        tr
            in
            ( model, randomize )

        StartPretest { spr, sc, vks, infos, acceptability } ->
            case acceptability of
                Result.Ok shuffledTrials ->
                    if List.filter (\block -> List.length block < 4) shuffledTrials == [ [] ] then
                        ( { model
                            | acceptability = Acceptability.start infos (List.concat shuffledTrials) model.version
                            , spr = SPR.init infos spr model.version
                            , sentenceCompletion = SentenceCompletion.init infos sc model.version
                            , vks =
                                { task = VKS.toActivity infos (List.filter (not << .isTraining) vks) model.version
                                , showVideo = False
                                }
                          }
                        , Cmd.none
                        )

                    else
                        update (ServerRespondedWithAllPretestData spr sc infos vks (List.concat shuffledTrials)) model

                Result.Err _ ->
                    ( { model | acceptability = Activity.Err "Error trying to build blocks" }, Cmd.none )



-- HTTP


attempt version =
    Para.attempt5
        { task1 = SPR.getRecords version
        , task2 = SentenceCompletion.getRecords
        , task3 = ActivityInfo.getRecords
        , task4 = VKS.getRecords
        , task5 = Acceptability.getRecords
        , onUpdates = ServerRespondedWithSomePretestData
        , onFailure = ServerRespondedWithSomeError
        , onSuccess = ServerRespondedWithAllPretestData
        }
