module Session exposing (Session(..))


type Session a
    = Loading a
    | Ready
    | NotAsked
    | Error String
