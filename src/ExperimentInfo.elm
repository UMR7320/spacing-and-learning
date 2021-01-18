module ExperimentInfo exposing (..)

import Data
import Http
import Json.Decode as Decode exposing (Decoder, string)
import Json.Decode.Pipeline exposing (custom, optional, required)
import Url.Builder


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


type alias Task =
    { uid : String
    , session : Session
    , type_ : Type_
    , name : String
    , url : String
    , description : String
    , instructions : String
    , instructions_short : String
    , end : String
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
                |> required "End" Decode.string
    in
    Data.decodeRecords decoder
