module Experiment.Scrabble exposing (..)

import Data
import Experiment.Experiment as Experiment
import Http exposing (Error)
import Json.Decode as Decode exposing (Decoder, string)
import Json.Decode.Pipeline exposing (..)


getTrialsFromServer : (Result Error (List Experiment.ScrabbleTrial) -> msg) -> Cmd msg
getTrialsFromServer msgHandler =
    Experiment.getTrialsFromServer_ "Scrabble" msgHandler decodeTranslationInput


decodeTranslationInput : Decoder (List Experiment.ScrabbleTrial)
decodeTranslationInput =
    let
        decoder =
            Decode.succeed Experiment.ScrabbleTrial
                |> required "UID" string
                |> required "Word_Text" string
                |> required "Word_Audio" Data.decodeAudioFiles
    in
    Data.decodeRecords decoder


initState : Experiment.ScrabbleState
initState =
    Experiment.ScrabbleState "DefaultUid" "" []


defaultTrial : Experiment.ScrabbleTrial
defaultTrial =
    Experiment.ScrabbleTrial "defaultTrial" "defaultTrial" (Data.AudioFile "" "")
