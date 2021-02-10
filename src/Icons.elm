module Icons exposing (helpCircle, music)

import Html exposing (Html)
import Html.Attributes
import Html.Events
import Svg exposing (Svg, svg)
import Svg.Attributes exposing (..)


svgFeatherIcon : String -> List (Svg msg) -> Html msg
svgFeatherIcon className =
    svg
        [ class <| "feather feather-" ++ className
        , fill "none"

        --, height "auto"
        , stroke "currentColor"
        , strokeLinecap "round"
        , strokeLinejoin "round"
        , strokeWidth "2"
        , viewBox "0 0 24 24"
        , width "100%"
        , class "cursor-pointer hover:opacity-75"
        ]


helpCircle : Html msg
helpCircle =
    svgFeatherIcon "help-circle"
        [ Svg.circle [ cx "12", cy "12", r "10" ] []
        , Svg.path [ d "M9.09 9a3 3 0 0 1 5.83 1c0 2-3 3-3 3" ] []
        , Svg.line [ x1 "12", y1 "17", x2 "12.01", y2 "17" ] []
        ]


music : Html msg
music =
    svgFeatherIcon "music"
        [ Svg.path [ d "M9 18V5l12-2v13" ] []
        , Svg.circle [ cx "6", cy "18", r "3" ] []
        , Svg.circle [ cx "18", cy "16", r "3" ] []
        ]
