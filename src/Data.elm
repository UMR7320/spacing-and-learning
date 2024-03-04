module Data exposing (..)

import Http exposing (Error(..), Response(..))
import Json.Decode as Decode exposing (..)
import Json.Decode.Pipeline exposing (required)
import RemoteData
import Url.Builder


splitIn : Int -> List a -> List (List a)
splitIn k xs =
    if k == 0 then
        [ [] ]

    else if k < 0 then
        []

    else if List.length xs > k then
        List.take k xs :: splitIn k (List.drop k xs)

    else
        [ xs ]


handleJsonResponse : Decoder a -> Http.Response String -> Result Http.Error a
handleJsonResponse decoder response =
    case response of
        Http.BadUrl_ url ->
            Err (Http.BadUrl url)

        Http.Timeout_ ->
            Err Http.Timeout

        Http.BadStatus_ { statusCode } _ ->
            Err (Http.BadStatus statusCode)

        Http.NetworkError_ ->
            Err Http.NetworkError

        Http.GoodStatus_ _ body ->
            case Decode.decodeString decoder body of
                Err error ->
                    Err (Http.BadBody (Decode.errorToString error))

                Ok result ->
                    Ok result


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


getGeneralParameters : (RemoteData.RemoteData Http.Error GeneralParameters -> c) -> Cmd c
getGeneralParameters responseHandler =
    Http.get
        { url =
            buildQuery
                { app = apps.spacing
                , base = "parameters"
                , view_ = "all"
                }
        , expect =
            Http.expectJson
                (RemoteData.fromResult >> responseHandler)
                decodeGeneralParameters
        }


decodeGeneralParameters : Decoder GeneralParameters
decodeGeneralParameters =
    let
        decoder =
            Decode.succeed GeneralParameters
                |> required "d_spacing (DAYS)" int
                |> required "m_spacing (DAYS)" int
                |> required "RI (DAYS)" int
                |> required "RI Surprise (DAYS)" int
                |> required "Consentement" string
    in
    decodeSingleRecord decoder


type alias GeneralParameters =
    { distributedSpacing : Int
    , massedSpacing : Int
    , retentionInterval : Int
    , retentionIntervalSurprise : Int
    , consent : String
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
    { spacing = "appk2Kc3YB4VZdGok"
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


decodeSingleRecord : Decoder a -> Decoder a
decodeSingleRecord xs =
    let
        decode fieldsDecoder =
            Decode.field "records" fieldsDecoder
    in
    decode (singleElementArrayDecoder xs)


singleElementArrayDecoder : Decoder a -> Decoder a
singleElementArrayDecoder decoder =
    list decoder
        |> andThen
            (\elements ->
                case elements of
                    element :: [] ->
                        succeed element

                    _ ->
                        fail "Expected exactly one element"
            )


type alias AudioFile =
    { url : String
    , type_ : String
    }


type UserCanParticipate
    = Yes String
    | No String


decodeUserCanParticipate : Decoder UserCanParticipate
decodeUserCanParticipate =
    field "userCanParticipate" bool
        |> andThen
            (\userCanParticipate ->
                if userCanParticipate then
                    map Yes (field "group" string)

                else
                    map No (field "reason" string)
            )


getCanUserParticipate : String -> (RemoteData.RemoteData Http.Error UserCanParticipate -> msg) -> Cmd msg
getCanUserParticipate userId msg =
    Http.get
        { url =
            Url.Builder.absolute
                [ ".netlify", "functions", "user-can-participate" ]
                [ Url.Builder.string "id" userId ]
        , expect = Http.expectJson (RemoteData.fromResult >> msg) decodeUserCanParticipate
        }
