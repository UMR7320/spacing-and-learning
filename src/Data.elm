module Data exposing (..)

import Json.Decode as Decode exposing (Decoder, string)
import Json.Decode.Pipeline exposing (optional, required)



{--
decodeTranslationInput : Decoder (List Experiment.TranslationInput)
decodeTranslationInput =
    let
        decoder =
            Decode.succeed Experiment.TranslationInput
                |> required "UID" string
                |> required "Question_Translation" string
                |> required "Distractor_1_Translation" string
                |> required "Distractor_2_Translation" string
                |> required "Distractor_3_Translation" string
    in
    decodeRecords decoder
--}


decodeAudioFiles : Decode.Decoder AudioFile
decodeAudioFiles =
    Decode.map (Maybe.withDefault (AudioFile "" "") << List.head) <|
        Decode.list audioDecoder


audioDecoder : Decode.Decoder AudioFile
audioDecoder =
    Decode.map2
        AudioFile
        (Decode.field "url" Decode.string)
        (Decode.field "type" Decode.string)


decodeRecords : Decoder a -> Decoder (List a)
decodeRecords xs =
    let
        decode fieldsDecoder =
            Decode.field "records" fieldsDecoder
    in
    decode (Decode.list xs)


type alias AudioFile =
    { url : String
    , type_ : String
    }



{--

packageDecoder : Decoder Package
packageDecoder =
    -- Build JSON decoders using the pipeline (|>) operator:
    -- https://package.elm-lang.org/packages/NoRedInk/elm-json-decode-pipeline/1.0.0/
    -- See also bool, float, int, list, nullable, etc. in:
    -- https://package.elm-lang.org/packages/elm/json/latest/Json-Decode
    Decode.succeed Package
        |> required "name" string
        |> required "url" string
        |> required "author" string
        |> required "license" string

--}
