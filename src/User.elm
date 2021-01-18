module User exposing (..)


type alias User =
    { uid : String
    , name : ( String, String )
    , email : String
    , tasksProgress : List Progress
    }


type Progress
    = NotStarted String
    | Doing String
    | Done String
