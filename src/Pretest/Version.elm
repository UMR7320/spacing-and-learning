module Pretest.Version exposing (Version(..), fromString, toString, toSession)

import ActivityInfo

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
toSession : Version -> ActivityInfo.Session
toSession version =
    case version of
        PreTest ->
            ActivityInfo.Pretest

        PostTest ->
            ActivityInfo.Posttest

        PostTestDiff ->
            ActivityInfo.PosttestDiff

        Surprise ->
            ActivityInfo.PosttestDiffSurprise

        Unknown _ ->
            ActivityInfo.OtherSession


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
