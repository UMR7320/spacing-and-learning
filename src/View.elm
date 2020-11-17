module View exposing
    ( button
    , container
    , header
    , keyValue
    , navIn
    , navOut
    , notFound
    , radio
    )

import Css
import Experiment.Experiment as E
import Html.Styled exposing (..)
import Html.Styled.Attributes
    exposing
        ( attribute
        , autofocus
        , checked
        , class
        , classList
        , css
        , href
        , id
        , name
        , target
        , type_
        )
import Html.Styled.Events exposing (onClick)


theme : { headerHeight : Css.Rem }
theme =
    { headerHeight = Css.rem 4 }



-- HEADER


header : List (Html msg) -> Html msg
header items =
    div
        [ class "fixed top-0 inset-x-0 bg-white border-b border-gray-300" -- Tailwind utilities: https://tailwindcss.com
        , css [ Css.height theme.headerHeight ] -- elm-css: https://package.elm-lang.org/packages/rtfeldman/elm-css/latest
        ]
        [ div
            [ class "container mx-auto h-full"
            , class "flex items-center px-6"
            ]
            [ a
                [ attribute "data-test" "logo"
                , class "flex items-center"
                , href "/"
                ]
                [ div
                    [ class "bg-indigo-600 w-4 h-4 rounded-full mr-2" ]
                    []
                , p
                    [ class "font-bold uppercase text-sm text-gray-800" ]
                    [ text "Apprentissage et Espacement" ]
                ]
            , ul
                [ attribute "data-test" "menu"
                , class "flex-grow"
                , class "flex justify-end"
                ]
                items
            ]
        ]


item : String -> List (Attribute msg) -> Html msg
item name attributes =
    li [ class "mr-6" ] [ a attributes [ text name ] ]


navIn : String -> String -> Html msg
navIn name url =
    item name [ href url ]


navOut : String -> String -> Html msg
navOut name url =
    item name [ href url, target "_blank", class "external" ]



-- CONTAINER


container : List (Html msg) -> Html msg
container content =
    div
        [ attribute "data-test" "content"
        , class "container mx-auto py-10 px-4"
        , css [ Css.marginTop theme.headerHeight ]
        ]
        content



-- DEFAULT PAGES


notFound : List (Html msg)
notFound =
    [ h1
        []
        [ div [ class "text-2xl text-gray-500" ] [ text "404" ]
        , text "Sorry, we could not find this page."
        ]
    , p
        []
        [ a
            [ class "btn", href "/" ]
            [ text "Home" ]
        ]
    ]



-- MISC


keyValue : String -> String -> List (Html msg)
keyValue key value =
    [ span
        [ class "inline-block mr-6 w-12"
        , class "text-gray-600 text-right text-xs uppercase"
        ]
        [ text key ]
    , span
        [ attribute "data-value" key ]
        [ text value ]
    ]


radio : String -> Bool -> Bool -> Bool -> msg -> Html msg
radio value isChecked isCorrect feedbackMode msg =
    label
        [ class "group block text-gray-70 font-medium ", id value ]
        [ div
            [ class
                ("border-solid border-2 px-4 py-4 mb-1 "
                    ++ (case ( feedbackMode, isChecked, isCorrect ) of
                            ( True, True, False ) ->
                                "border-red-500 line-through"

                            ( True, True, True ) ->
                                "border-green-500"

                            ( False, True, _ ) ->
                                "border-indigo-600"

                            ( False, False, _ ) ->
                                "border-grey-500"

                            ( True, False, _ ) ->
                                "border-grey-500"
                       )
                )
            , if feedbackMode == False then
                class "group-hover:border-indigo-600 hover:underline cursor-pointer"

              else
                class ""
            , class "active:border-indigo-400"
            ]
            [ input
                [ type_ "radio"
                , checked isChecked
                , name "definition-choice"
                , onClick msg
                , Html.Styled.Attributes.disabled feedbackMode
                , autofocus isCorrect
                ]
                []
            , span [ class "pl-4 " ] [ text value ]
            ]
        ]


button : { message : msg, txt : String, isDisabled : Bool } -> Html msg
button { message, txt, isDisabled } =
    Html.Styled.button
        [ class "max-w-xl w-full mt-6"
        , onClick message
        , Html.Styled.Attributes.disabled isDisabled
        , class <|
            if isDisabled then
                "invisible"

            else
                "visible"
        ]
        [ text txt ]
