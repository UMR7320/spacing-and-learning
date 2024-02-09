module Session exposing (Info, Session(..), getInfos)

import Data
import Dict exposing (Dict)
import Http exposing (expectJson)
import Json.Decode as Decode exposing (Decoder)
import Json.Decode.Pipeline exposing (optional, required)
import RemoteData exposing (RemoteData)


type Session a
    = Loading a
    | Ready
    | NotAsked
    | Error String


getInfos : (RemoteData Http.Error (Dict String Info) -> c) -> Cmd c
getInfos handler =
    Http.get
        { url = Data.buildQuery { app = "appvKOc8FH0j48Hw1", base = "sessions", view_ = "" }
        , expect = expectJson (RemoteData.fromResult >> handler) decode
        }


type alias Info =
    { name : String
    , instructions : String
    }


decode : Decoder (Dict String Info)
decode =
    let
        decoder =
            Decode.succeed Info
                |> required "Name" Decode.string
                |> optional "Instructions" Decode.string "Missing session instructions"
    in
    Data.decodeRecords decoder
        |> Decode.andThen
            (\decodedList ->
                Decode.succeed
                    (List.map
                        (\session ->
                            ( session.name, identity session )
                        )
                        decodedList
                        |> Dict.fromList
                    )
            )
