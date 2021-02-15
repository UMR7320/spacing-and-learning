module Experiment.Scrabble exposing (..)

import Data
import Http exposing (Error)
import Json.Decode as Decode exposing (Decoder, string)
import Json.Decode.Pipeline exposing (..)


getTrialsFromServer : (Result Error (List Trial) -> msg) -> Cmd msg
getTrialsFromServer msgHandler =
    Data.getTrialsFromServer_ "input" "SpellingLvl2" msgHandler decodeTranslationInput


decodeTranslationInput : Decoder (List Trial)
decodeTranslationInput =
    let
        decoder =
            Decode.succeed Trial
                |> required "UID" string
                |> required "Word_Text" string
                |> required "Word_Audio" Data.decodeAudioFiles
                |> Data.decodeBool "isTraining"
                |> required "Word_Text" string
    in
    Data.decodeRecords decoder


initState : State
initState =
    State "DefaultUid" "" []


defaultTrial : Trial
defaultTrial =
    Trial "defaultTrial" "defaultTrial" (Data.AudioFile "" "") False ""


type alias Trial =
    { uid : String
    , writtenWord : String
    , audioWord : Data.AudioFile
    , isTraining : Bool
    , target : String
    }


type alias State =
    { uid : String
    , userAnswer : String
    , scrambledLetter : List KeyedItem
    }


type alias Item =
    String


type alias KeyedItem =
    ( String, Item )


dedupeHelper : List String -> List ( String, Int ) -> List ( String, Int )
dedupeHelper letters acc =
    let
        lettersInAcc =
            List.map Tuple.first acc

        countRecLetters target =
            List.foldr
                (\letter acc_ ->
                    if target == letter then
                        acc_ + 1

                    else
                        acc_
                )
                1
                lettersInAcc
    in
    case letters of
        [] ->
            acc |> List.reverse

        x :: xs ->
            if List.member x lettersInAcc then
                dedupeHelper xs <|
                    ( x
                    , countRecLetters x
                    )
                        :: acc

            else
                dedupeHelper xs <| ( x, 1 ) :: acc


dedupe : List String -> List ( String, Int )
dedupe letters =
    dedupeHelper letters []
