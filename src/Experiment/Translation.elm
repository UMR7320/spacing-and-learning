module Experiment.Translation exposing (..)

import Data
import Experiment.Experiment as Experiment
import Json.Decode as Decode exposing (Decoder, string)
import Json.Decode.Pipeline exposing (..)


getTrialsFromServer : a -> b
getTrialsFromServer msgHandler =
    Experiment.getTrialsFromServer_ "Translation" msgHandler decodeTrials


type alias Trial =
    {}


type alias State =
    {}


decodeTrials : Decoder (List Trial)
decodeTrials =
    Data.decodeRecords (Decode.succeed Trial)


initState : State
initState =
    State
