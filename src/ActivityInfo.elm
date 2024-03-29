module ActivityInfo exposing (..)

import Data
import Dict
import Http
import Json.Decode as Decode
import Json.Decode.Pipeline exposing (custom, optional, required)
import Task exposing (Task)


getInfos : (Result Http.Error (List ActivityInfo) -> msg) -> Cmd msg
getInfos toMsg =
    Data.getTrialsFromServer_ "tasks" "allActivityInfosGrid" toMsg decode


type Session
    = Session1
    | Session2
    | Session3
    | Pretest
    | Posttest
    | PosttestDiff
    | PosttestDiffSurprise
    | OtherSession


sessionToString : Session -> String
sessionToString str =
    case str of
        Session1 ->
            "Session1"

        Session2 ->
            "Session 2"

        Session3 ->
            "Session 3"

        Pretest ->
            "Pretest"

        Posttest ->
            "Post-test"

        PosttestDiff ->
            "Post-test-diff"

        PosttestDiffSurprise ->
            "Post-test-diff-surprise"

        OtherSession ->
            "Other"


type Type_
    = Sens
    | Forme
    | Context
    | Other


typeToString : Type_ -> String
typeToString t =
    case t of
        Sens ->
            "Sens"

        Forme ->
            "Forme"

        Context ->
            "Context"

        Other ->
            "Other"


type alias Description =
    String


toDict newInfos =
    newInfos
        |> List.map
            (\info ->
                ( info.uid
                , identity info
                )
            )
        |> Dict.fromList


activityInfo : List ActivityInfo -> Session -> String -> Result String ActivityInfo
activityInfo infos session name =
    infos
        |> List.filter (\task -> task.session == session && task.name == name)
        |> List.head
        |> Result.fromMaybe ("Could not find " ++ name ++ " info for session " ++ sessionToString session)


type alias ActivityInfo =
    { uid : String
    , session : Session
    , type_ : Type_
    , name : String
    , url : String
    , description : String
    , instructions : String
    , instructions_short : String
    , feedback_correct : String
    , feedback_incorrect : String
    , end : String
    , trainingWheel : String
    , introToMain : String
    }


decode : Decode.Decoder (List ActivityInfo)
decode =
    let
        mapToSession : String -> Decode.Decoder Session
        mapToSession str =
            case str of
                "session1" ->
                    Decode.succeed Session1

                "session2" ->
                    Decode.succeed Session2

                "session3" ->
                    Decode.succeed Session3

                "Prétest" ->
                    Decode.succeed Pretest

                "Post-test" ->
                    Decode.succeed Posttest

                "Post-test-diff" ->
                    Decode.succeed PosttestDiff

                "Post-test-diff-surprise" ->
                    Decode.succeed PosttestDiffSurprise

                _ ->
                    Decode.succeed OtherSession

        mapToType_ : String -> Decode.Decoder Type_
        mapToType_ str =
            case str of
                "Sens" ->
                    Decode.succeed Sens

                "Forme" ->
                    Decode.succeed Forme

                "Context" ->
                    Decode.succeed Context

                _ ->
                    Decode.succeed Other

        decoder =
            Decode.succeed ActivityInfo
                |> required "id" Decode.string
                |> custom (Decode.field "Session" Decode.string |> Decode.andThen mapToSession)
                |> custom (Decode.field "Type" Decode.string |> Decode.andThen mapToType_)
                |> optional "Name" Decode.string "Missing Name"
                |> optional "Demo_Link" Decode.string "Missing link"
                |> optional "Description" Decode.string "Missing Description"
                |> optional "Instructions" Decode.string "Missing instructions"
                |> optional "Instructions_short" Decode.string "Missing instructions short"
                |> optional "feedback_correct" Decode.string "Missing feedback correct"
                |> optional "feedback_incorrect" Decode.string "Missing feedback incorrect"
                |> optional "End" Decode.string "Missing End"
                |> optional "trainingWheels" Decode.string "Missing training wheel"
                |> optional "IntroToMain" Decode.string "Missing IntroToMain text"
    in
    Data.decodeRecords decoder


getRecords : Task Http.Error (List ActivityInfo)
getRecords =
    Http.task
        { method = "GET"
        , headers = []
        , url =
            Data.buildQuery
                { app = Data.apps.spacing
                , base = "tasks"
                , view_ = "allTasksGrid"
                }
        , body = Http.emptyBody
        , resolver = Http.stringResolver <| Data.handleJsonResponse <| decode
        , timeout = Just 5000
        }
