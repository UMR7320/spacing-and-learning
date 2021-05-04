module Session exposing (Session(..))


type Session a
    = Loading a
    | FailedToLoad String
    | Ready
    | NotAsked
