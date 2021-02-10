module Data exposing (..)

import Http
import Json.Decode as Decode exposing (Decoder, string)
import Json.Decode.Pipeline exposing (custom, optional, required)
import Url.Builder


buildErrorMessage : Http.Error -> String
buildErrorMessage httpError =
    case httpError of
        Http.BadUrl message ->
            message

        Http.Timeout ->
            "Server is taking too long to respond. Please try again later."

        Http.NetworkError ->
            "Unable to reach server."

        Http.BadStatus statusCode ->
            "Request failed with status code: " ++ String.fromInt statusCode

        Http.BadBody message ->
            message


getTrialsFromServer_ : String -> String -> (Result Http.Error a -> msg) -> Decode.Decoder a -> Cmd msg
getTrialsFromServer_ baseName viewName callbackMsg decoder =
    Http.get
        { url =
            buildQuery
                { app = apps.spacing
                , base = baseName
                , view_ = viewName
                }
        , expect =
            Http.expectJson
                callbackMsg
                decoder
        }


decodeBool =
    let
        stringToBoolDecoder : String -> Decode.Decoder Bool
        stringToBoolDecoder str =
            case str of
                "true" ->
                    Decode.succeed True

                _ ->
                    Decode.succeed False
    in
    custom (Decode.field "isTraining" Decode.string |> Decode.andThen stringToBoolDecoder)


sendUserData : Http.Body -> (Result Http.Error a -> msg) -> Decoder a -> Cmd msg
sendUserData payload callbackMsg decoder =
    Http.post
        { url =
            buildQuery
                { app = apps.spacing
                , base = "users"
                , view_ = "allUsers"
                }
        , body = payload
        , expect = Http.expectJson callbackMsg decoder
        }


type alias AirtableQueryParameters =
    { app : String
    , base : String
    , view_ : String
    }


buildQuery : AirtableQueryParameters -> String
buildQuery { app, base, view_ } =
    Url.Builder.absolute
        [ ".netlify"
        , "functions"
        , "api"
        ]
        [ Url.Builder.string "app" app
        , Url.Builder.string "base" base
        , Url.Builder.string "view" view_
        ]


apps : { spacing : String, sleep : String }
apps =
    { spacing = "appvKOc8FH0j48Hw1"
    , sleep = "appTEVHZLw3jNa7fU"
    }


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
