port module User exposing (..)

import Json.Decode
import Json.Decode.Pipeline as Decode
import Json.Encode as Encode


port storeInfo : String -> Cmd msg


type Role
    = Volunteer
    | Admin


type User
    = Authenticated { uid : String, firstName : String, email : String, role : Role }


type alias AnoInfo =
    { firstName : String
    , email : String
    , isStudent : Bool
    , degree : Maybe String
    , languages : List String
    , handedness : Handedness
    , age : Int
    , gender : Gender
    , ageFirstExposureToEnglish : Int
    , englishClassesNumber : Int
    , profeciency : Profeciency
    , englisheUse : List ( Activity, Frequency )
    , englishExposure : List ( String, String, String )
    , otherLanguages : List ( String, Int, Profeciency )
    }


type Activity
    = Films
    | Music
    | Read
    | Speak
    | LearningActivities


type Frequency
    = Everyday
    | SeveralTimesAWeek
    | OnceAWeek
    | OnceTwiceAMonth
    | Rarely


type Profeciency
    = A1
    | A2
    | B1
    | B2
    | C1
    | C2


type Gender
    = Male
    | Female
    | Other
    | NoAnswer


type Handedness
    = Right
    | Left
    | Both


type alias AuthenticatedInfo =
    { uid : String, firstName : String, email : String, role : Role }


encoder : User -> Encode.Value
encoder user =
    case user of
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
