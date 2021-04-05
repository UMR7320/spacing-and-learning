module Progressbar exposing (..)

import Css
import Css.Transitions exposing (transition)
import Html.Styled as Html exposing (Html)
import Html.Styled.Attributes as Html exposing (class, style)
import Tuple
import View exposing (pct)


progressBar : List ( trial, state ) -> List trial -> Html msg
progressBar history remainingTrials =
    let
        trialNumber =
            List.length history + 1

        pct_ =
            View.pct trialNumber (remainingTrials ++ List.map Tuple.first history)
    in
    Html.div
        [ class "shadow max-w-xl w-full bg-gray-300 mt-2 mb-12 transition duration-500"
        ]
        [ Html.div
            [ class "bg-indigo-600 text-sm font-bold w-full leading-none py-1 text-center text-white "
            , Html.css
                [ Css.width (Css.pct pct_)
                ]
            ]
            [ Html.text <| String.fromInt (round pct_) ++ "%" ]
        ]
