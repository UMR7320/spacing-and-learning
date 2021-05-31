module Pretest.YesNo exposing (..)

import Data
import Http
import Json.Decode as Decode
import Json.Decode.Pipeline exposing (..)


bla =
    ""


getRecords =
    Http.task
        { method = "GET"
        , headers = []
        , url =
            Data.buildQuery
                { app = Data.apps.spacing
                , base = "yes_no"
                , view_ = "all"
                }
        , body = Http.emptyBody
        , resolver = Http.stringResolver <| Data.handleJsonResponse <| decodeAcceptabilityTrials
        , timeout = Just 5000
        }


decodeAcceptabilityTrials : Decode.Decoder (List Trial)
decodeAcceptabilityTrials =
    let
        decoder =
            Decode.succeed Trial
                |> required "id" Decode.string
                |> required "ItemName" Decode.string
                |> optional "Exists" Decode.bool False
    in
    Data.decodeRecords decoder


type alias State =
    { evaluation : Bool }


type alias Trial =
    { id : String, word : String, exists : Bool }


update msg model =
    ( model, Cmd.none )


type Msg
    = NoOp


view task =
    []
