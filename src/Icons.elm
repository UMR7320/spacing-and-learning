module Icons exposing (..)

import Html exposing (Html)
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
        ]


checkCircle : Html msg
checkCircle =
    svgFeatherIcon "check-circle"
        [ Svg.path [ d "M22 11.08V12a10 10 0 1 1-5.93-9.14" ] []
        , Svg.polyline [ points "22 4 12 14.01 9 11.01" ] []
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
    svgFeatherIcon "play-circle"
        [ Svg.circle [ cx "12", cy "12", r "10" ] []
        , Svg.polygon [ points "10 8 16 12 10 16 10 8" ] []
        ]


xCircle : Html msg
xCircle =
    svgFeatherIcon "x-circle"
        [ Svg.circle [ cx "12", cy "12", r "10" ] []
        , Svg.line [ x1 "15", y1 "9", x2 "9", y2 "15" ] []
        , Svg.line [ x1 "9", y1 "9", x2 "15", y2 "15" ] []
        ]


chevronRight : Html msg
chevronRight =
    svgFeatherIcon "chevron-right"
        [ Svg.polyline [ points "9 18 15 12 9 6" ] []
        ]


chevronDown : Html msg
chevronDown =
    svgFeatherIcon "chevron-down"
        [ Svg.polyline [ points "6 9 12 15 18 9" ] []
        ]
