module Pretest.YesNo exposing (..)


bla =
    ""


type alias State =
    { evaluation : Bool }


type alias Trial =
    { id : String, word : String, exists : Bool }


update msg model =
    ( model, Cmd.none )


type Msg
    = NoOp


view task =
    []
