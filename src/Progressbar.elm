module Progressbar exposing (..)

import Css
import Css.Transitions exposing (transition)
import Html.Styled as Html exposing (Html)
import Html.Styled.Attributes as Html exposing (class, style)


progressBar : Float -> Html msg
progressBar percentage =
    Html.div
        [ class "shadow max-w-xl bg-gray-300 mt-2"
        ]
        [ Html.div
            [ class "bg-indigo-600 text-sm font-bold leading-none py-1 text-center text-white "
            , Html.css
                [ Css.width (Css.pct percentage)
                ]
            ]
            [ Html.text <| String.fromInt (round percentage) ++ "%" ]
        ]
