module Pretest.GeneralInfos exposing (..)

import Html.Styled exposing (div, h1, input, label, text)
import Html.Styled.Attributes exposing (class, for, id, type_, value)
import Html.Styled.Events
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
    = UserUpdatedEmailField String


type alias Volunteer =
    { email : String }
