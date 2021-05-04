module Pretest.GeneralInfos exposing (..)

import Data
import Dict
import ExperimentInfo exposing (Task)
import Html.Styled as Html exposing (Html, div, fromUnstyled, h1, h2, input, label, p, span, text)
import Html.Styled.Attributes exposing (class, for, id, type_, value)
import Html.Styled.Events
import Http exposing (Error)
import Icons
import Json.Decode as Decode exposing (Decoder, string)
import Json.Decode.Pipeline exposing (..)
import Logic
import String.Interpolate exposing (interpolate)
import View


view model userUpdatedEmailField sendUserDataMsg =
    div [ class "container flex flex-col items-center justify-center" ]
        [ h1 [] [ text "Hello" ]
        , label [ for "email" ] [ text "Enter your email" ]
        , input [ type_ "email", value model, id "email", class "border-2 p-2", Html.Styled.Events.onInput userUpdatedEmailField ] []
        , View.button { isDisabled = False, message = sendUserDataMsg model, txt = "S'enregistrer" }
        ]


type alias Model =
    String


type Msg
    = UserClickedSendData String
    | UserUpdatedEmailField String
    | UserCreated (Result Http.Error ())


type alias Volunteer =
    { email : String }
