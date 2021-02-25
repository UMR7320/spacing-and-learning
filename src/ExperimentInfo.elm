module ExperimentInfo exposing (..)

import Data
import Dict
import Http
import Json.Decode as Decode exposing (Decoder, string)
import Json.Decode.Pipeline exposing (custom, optional, required)


getInfos : (Result Http.Error (List Task) -> msg) -> Cmd msg
getInfos toMsg =
    Data.getTrialsFromServer_ "tasks" "allTasksGrid" toMsg decode


type Session
    = Session1
    | Session2
    | Session3
    | Pretest


sessionToString str =
    case str of
        Session1 ->
            "Session1"

        Session2 ->
            "Session 2"

        Session3 ->
            "Session 3"

        Pretest ->
            "Pretest "


type Type_
    = Sens
    | Forme
    | Context
    | Other


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
                , { uid = info.uid
                  , session = info.session
                  , type_ = info.type_
                  , name = info.name
                  , url = info.url
                  , description = info.description
                  , instructions = info.instructions
                  , instructions_short = info.instructions_short
                  , end = info.end
                  , feedback_correct = info.feedback_correct
                  , feedback_incorrect = info.feedback_incorrect
                  , trainingWheel = info.trainingWheel
                  }
                )
            )
        |> Dict.fromList


type alias Task =
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
    }


decode : Decode.Decoder (List Task)
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

                _ ->
                    Decode.fail "I couldn't map this to a Session"

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
            Decode.succeed Task
                |> required "UID" Decode.string
                |> custom (Decode.field "Session" Decode.string |> Decode.andThen mapToSession)
                |> custom (Decode.field "Type" Decode.string |> Decode.andThen mapToType_)
                |> required "Name" Decode.string
                |> required "Demo_Link" Decode.string
                |> required "Description" Decode.string
                |> required "Instructions" Decode.string
                |> required "Instructions_short" Decode.string
                |> required "feedback_correct" Decode.string
                |> required "feedback_incorrect" Decode.string
                |> required "End" Decode.string
                |> optional "trainingWheels" Decode.string "Missing training wheel"
    in
    Data.decodeRecords decoder


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
