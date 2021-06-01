port module Ports exposing (..)


port playAudio : String -> Cmd msg


port audioEnded : ({ eventType : String, name : String, timestamp : Int } -> msg) -> Sub msg
