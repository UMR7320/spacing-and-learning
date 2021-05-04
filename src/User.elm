port module User exposing (..)

import Json.Decode
import Json.Decode.Pipeline as Decode
import Json.Encode as Encode


port storeInfo : String -> Cmd msg


type Role
    = Volunteer
    | Admin


type User
    = Ano { firstName : String, email : String }
    | Authenticated { uid : String, firstName : String, email : String, role : Role }


type alias AnoInfo =
    { firstName : String, email : String }


type alias AuthenticatedInfo =
    { uid : String, firstName : String, email : String, role : Role }


encoder : User -> Encode.Value
encoder user =
    case user of
        Ano info ->
            Encode.object
                [ ( "First Name", Encode.string info.firstName )
                , ( "Email", Encode.string info.email )
                , ( "role", Encode.string "Volunteer" )
                ]

        Authenticated info ->
            Encode.object
                [ ( "First Name", Encode.string info.firstName )
                , ( "Email", Encode.string info.email )
                , ( "role", Encode.string (fromRole info.role) )
                , ( "uid", Encode.string info.uid )
                ]


fromRole role =
    case role of
        Admin ->
            "Admin"

        Volunteer ->
            "Volunteer"


mapToRole str =
    case str of
        "Admin" ->
            Json.Decode.succeed Admin

        "Volunteer" ->
            Json.Decode.succeed Volunteer

        _ ->
            Json.Decode.fail "I tried to map the newly decoded user to a role but I ran into an unexpected case. Check in the DB that the spelling is ok."


decodeAuthenticatedInfo =
    Json.Decode.succeed AuthenticatedInfo
        |> Decode.required "UID" Json.Decode.string
        |> Decode.required "First Name" Json.Decode.string
        |> Decode.required "Email" Json.Decode.string
        |> Decode.custom (Json.Decode.field "role" Json.Decode.string |> Json.Decode.andThen mapToRole)


decodeAnoInfo =
    Json.Decode.succeed AnoInfo
        |> Decode.required "First Name" Json.Decode.string
        |> Decode.required "Email" Json.Decode.string
