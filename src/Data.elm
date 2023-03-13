module Data exposing (..)

import Dict
import Http exposing (Error(..), Response(..))
import Json.Decode as Decode exposing (..)
import Json.Decode.Pipeline exposing (custom, optional, required)
import Json.Encode as Encode
import Process
import RemoteData
import Task
import Url.Builder
import Html exposing (thead)


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


tasksIds =
    [ "rec9fDmVOpqDJktmQ"
    , "recf5HANE632FLKbc"
    , "recB3kUQW4jNTlou6"
    , "recJOpE5pMTCHJOSV"
    , "recSL8cthViyXRx8u"
    , "recJucOXEZzJj6Uui"
    , "recsN8oyy3LIC8URx"
    , "recwxsmowpB18bpLj"
    , "recFEtKbtuBSolHnI"
    , "rec4LrrQtHVQqnHQp"
    , "rechYdq4MyLcb2nRG"
    , "recR8areYkKRvQ6lU"
    , "reczQs5ZD6g1x5F29"
    , "rec8eKMwCMFFtKVKD"
    ]


tasksLabels =
    [ "Meaning"
    , "Translation"
    , "Synonym"
    , "Spelling level 1"
    , "Spelling level 2"
    , "Spelling level 3"
    , "Context Understanding level 1"
    , "Context Understanding level 2"
    , "Context Understanding level 3"
    , "CloudWords"
    , "YesNo task"
    , "acceptability"
    , "Pretest Sentence Completion"
    , "Presentation"
    ]


tasks =
    List.map2 Tuple.pair tasksLabels tasksIds |> Dict.fromList


{--}
sendInBatch : (List history -> Encode.Value) -> String -> String -> List history -> Task.Task Http.Error (List ())
sendInBatch historyEncoder taskId userId history =
    let
        chuncks =
            splitIn 10 history

        fieldsToUpdate =
            Encode.object [ ( "tasks", Encode.list Encode.string [ taskId ] ) ]
    in
    chuncks
        |> List.map
            (\sublist ->
                Process.sleep 0
                    |> Task.andThen
                        (\_ -> postRecordsBatch (Http.jsonBody <| historyEncoder sublist))
            )
        {--|> (::)
            (updateUserCompletedTasks
                (Http.jsonBody <|
                    Encode.object
                        [ ( "id", Encode.string userId )
                        , ( "fields", fieldsToUpdate )
                        ]
                )
            )--}
        |> Task.sequence



--|> Task.map List.concat
--}


postRecordsBatch : Http.Body -> Task.Task Http.Error ()
postRecordsBatch payload =
    Http.task
        { method = "POST"
        , headers = []
        , url =
            buildQuery
                { app = apps.spacing
                , base = "output"
                , view_ = "all"
                }
        , body = payload
        , resolver = Http.stringResolver <| resolve <| always (Ok ()) --<| handleJsonResponse <| decode
        , timeout = Just 5000
        }


resolve : (body -> Result String a) -> Http.Response body -> Result Http.Error a
resolve bodyParser response =
    case response of
        BadUrl_ url ->
            Result.Err (BadUrl url)

        Timeout_ ->
            Err Timeout

        NetworkError_ ->
            Err NetworkError

        BadStatus_ metadata _ ->
            Err (BadStatus metadata.statusCode)

        GoodStatus_ _ body ->
            Result.mapError BadBody (bodyParser body)


updateUserCompletedTasks : Http.Body -> Task.Task Http.Error ()
updateUserCompletedTasks payload =
    Http.task
        { method = "PATCH"
        , headers = []
        , url =
            buildQuery
                { app = apps.spacing
                , base = "users"
                , view_ = "all"
                }
        , body = payload
        , resolver = Http.stringResolver <| resolve <| always (Ok ()) -- Http.stringResolver <| handleJsonResponse <| decode
        , timeout = Just 5000
        }


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
                Err _ ->
                    Err (Http.BadBody body)

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


decodeBool : String -> Decoder (Bool -> b) -> Decoder b
decodeBool fieldname =
    let
        stringToBoolDecoder : String -> Decode.Decoder Bool
        stringToBoolDecoder str =
            case str of
                "true" ->
                    Decode.succeed True

                _ ->
                    Decode.succeed False
    in
    custom (Decode.field fieldname Decode.string |> Decode.andThen stringToBoolDecoder)


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


type UserCanParticipate = Yes | No String


decodeUserCanParticipate : Decoder UserCanParticipate
decodeUserCanParticipate =
    field "userCanParticipate" bool
    |> andThen (\userCanParticipate ->
        if userCanParticipate then
            succeed Yes
        else
            map No (field "reason" string)
      )


getCanUserParticipate : String -> (RemoteData.RemoteData Http.Error UserCanParticipate -> msg) -> Cmd msg
getCanUserParticipate userId msg =
    Http.get
        { url = Url.Builder.absolute
            [ ".netlify" , "functions" , "user-can-participate"]
            [ Url.Builder.string "id" userId ]
        , expect = Http.expectJson (RemoteData.fromResult >> msg) decodeUserCanParticipate
        }
