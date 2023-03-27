module Pretest.Version exposing (Version(..), fromString, toString, toSession)

import ExperimentInfo

type Version
    = PreTest
    | PostTest
    | PostTestDiff
    | Surprise
    | Unknown String


fromString : String -> Version
fromString version =
    case version of
        "pre" ->
            PreTest

        "post" ->
            PostTest

        "post-diff" ->
            PostTestDiff

        "surprise" ->
            Surprise

        val ->
            Unknown val


-- version and session should probably be merged, but the pages are not well organized
-- so this is non-trivial
toSession : Version -> ExperimentInfo.Session
toSession version =
    case version of
        PreTest ->
            ExperimentInfo.Pretest

        PostTest ->
            ExperimentInfo.Posttest

        PostTestDiff ->
            ExperimentInfo.PosttestDiff

        Surprise ->
            ExperimentInfo.PosttestDiffSurprise

        Unknown val ->
            ExperimentInfo.OtherSession


toString : Version -> String
toString version =
    case version of
        PreTest ->
            "pre"

        PostTest ->
            "post"

        PostTestDiff ->
            "post-diff"

        Surprise ->
            "surprise"

        Unknown val ->
            val
