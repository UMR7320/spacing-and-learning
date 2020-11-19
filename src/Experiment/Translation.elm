module Experiment.Translation exposing (..)

import Data
import Experiment.Experiment as Experiment
import Http exposing (Error)
import Json.Decode as Decode exposing (Decoder, string)
import Json.Decode.Pipeline exposing (..)



--getTrialsFromServer : Decodera -> b


getTrialsFromServer : (Result Error (List Experiment.TranslationInput) -> msg) -> Cmd msg
getTrialsFromServer msgHandler =
    Experiment.getTrialsFromServer_ "Translation" msgHandler decodeTranslationInput


type alias Trial =
    {}


type alias State =
    {}


decodeTranslationInput : Decoder (List Experiment.TranslationInput)
decodeTranslationInput =
    let
        decoder =
            Decode.succeed Experiment.TranslationInput
                |> required "UID" string
                |> required "Question_Translation" string
                |> required "Translation_1" string
                |> optional "Translation_2" string "MISSING_TRANS_2"
                |> optional "Distractor_1_Translation" string "Missing distractor"
                |> optional "Distractor_2_Translation" string "Missing distractor"
                |> optional "Distractor_3_Translation" string "missing distractor"
                |> optional "Distractor_4_Translation" string "missing distractor"
                |> optional "Word_Text" string "MISSING"
    in
    Data.decodeRecords decoder


initState =
    { uid = ""
    , question = ""
    , option1 = ""
    , option2 = ""
    , word = ""
    }
