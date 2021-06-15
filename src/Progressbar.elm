module Progressbar exposing (..)

import Css
import Html.Styled as Html exposing (Html)
import Html.Styled.Attributes as Html exposing (class)
import Tuple
import View


progressBar : List ( { trial | isTraining : Bool }, state ) -> List { trial | isTraining : Bool } -> Html msg
progressBar history remainingTrials =
    let
        trialNumber =
            List.length filteredHistory + 1

        filteredHistory =
            List.filter (not << .isTraining << Tuple.first) history

        pct_ =
            View.pct trialNumber (remainingTrials ++ List.map Tuple.first filteredHistory)
    in
    viewProgressBar pct_


progressBarWhenNoTraining : List ( trial, state ) -> List trial -> Html msg
progressBarWhenNoTraining history remainingTrials =
    let
        trialNumber =
            List.length history + 1

        pct_ =
            View.pct trialNumber (remainingTrials ++ List.map Tuple.first history)
    in
    viewProgressBar pct_


viewProgressBar pct_ =
    Html.div
        [ class "shadow max-w-xl w-full bg-gray-300 mt-2 mb-2 transition duration-500"
        ]
        [ Html.div
            [ class "bg-indigo-600 text-sm font-bold w-full leading-none py-1 text-center text-white "
            , Html.css
                [ Css.width (Css.pct pct_)
                ]
            ]
            []
        ]
