module View exposing
    ( button
    , container
    , floatingLabel
    , fromMarkdown
    , header
    , keyValue
    , navIn
    , navOut
    , notFound
    , pct
    , radio
    , sentenceInSynonym
    , shuffledOptions
    , simpleAudioPlayer
    , tooltip
    , viewFeedbackInSingleChoice
    , viewInstructions
    , viewQuestion
    , viewTraining
    )

import Array
import Css
import Css.Global
import Css.Transitions
import Experiment.Experiment as E
import ExperimentInfo
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
import Icons
import Markdown
import PsychTask


shuffledOptions state fb radioMsg trial optionsOrder =
    let
        option id =
            radio
                id
                (state.userAnswer == id)
                (isCorrect id)
                fb
                (radioMsg id)

        isCorrect optionN =
            optionN == trial.target

        options =
            [ option trial.distractor1
            , option trial.distractor2
            , option trial.distractor3
            , option trial.target
            ]

        ordoredOptions =
            options
                |> List.map2 Tuple.pair optionsOrder
                |> List.sortBy Tuple.first
                |> List.map Tuple.second
    in
    ordoredOptions


viewQuestion : Int -> { a | target : String } -> String -> Html msg
viewQuestion trialn trial instructions =
    h3 []
        [ p []
            [ text <| String.fromInt (trialn + 1) ++ ". " ++ instructions
            , span [ class "italic" ] [ text <| trial.target ]
            ]
        ]


viewFeedbackInSingleChoice : Bool -> { a | userAnswer : String } -> { b | target : String } -> Html msg
viewFeedbackInSingleChoice isVisible state trial =
    p
        [ class
            ("font-medium py-4 w-full"
                ++ " "
                ++ (if isVisible then
                        "visible"

                    else
                        "invisible"
                   )
            )
        ]
        (if state.userAnswer == trial.target then
            [ text "✔️ Correct Answer ! " ]

         else
            [ text "❌ The correct definition is : "
            , span [ class "font-medium italic" ] [ text trial.target ]
            ]
        )



--TRAINING


trainingBox : List (Html msg) -> Html msg
trainingBox =
    div [ class "grid grid-cols-6 mx-auto w-full h-full border-4 border-green-500 border-rounded-lg border-dashed " ]


viewInstructions : String -> Html msg
viewInstructions instructions =
    div [ class "flex flex-col" ]
        [ h2 [ class "font-bold" ] [ text "Instructions" ]
        , p [ class "pt-8 pb-8 font-medium" ]
            [ pre [] [ text instructions ]
            ]
        , div [ class "text-lg text-green-500 font-bold pb-2" ] [ span [] [ text "Practice here !" ] ]
        ]


trainingWheels : Int -> String -> String -> Html msg
trainingWheels trialn radical target =
    let
        helpSentence =
            div [ class "flex flex-col pt-4 italic text-xl " ]
                [ p []
                    [ text "The synonym of "
                    , span [ class "font-bold" ] [ text radical ]
                    , text " is "
                    , span [ class "font-bold" ] [ text target ]
                    ]
                , span [] [ text "Type it here and you're good to go!" ]
                ]
    in
    case trialn of
        0 ->
            helpSentence

        _ ->
            div [] []


viewTraining : String -> List (Html msg) -> Html msg
viewTraining instructions content =
    div [ class "flex flex-col" ]
        [ viewInstructions instructions, trainingBox content ]


pct : Int -> List a -> Float
pct trialn trials =
    (toFloat trialn / toFloat (List.length trials)) * 100


theme : { headerHeight : Css.Rem }
theme =
    { headerHeight = Css.rem 4 }



{--
feedback_ : { attempt : String, stimulus : String, target : String } -> String -> Html msg
feedback_ { attempt, stimulus, target } str =
    let
        feedback_ =
            String.words str

        injectVariable word =
            case word of
                "attempt" ->
                    text attempt

                "stimulus" ->
                    text stimulus

                "target" ->
                    text target

                _ ->
                    text word
    in
    feedback_
        |> List.map injectVariable
        |> div []

--}
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
                ]
                []
            , span [ class "pl-4 " ] [ text value ]
            ]
        ]


fromMarkdown =
    fromUnstyled << Markdown.toHtml []


floatingLabel : String -> String -> (String -> msg) -> Bool -> Html msg
floatingLabel stim val msg givenIsFeedback =
    Html.Styled.div
        [ Html.Styled.Attributes.css
            [ Css.position Css.relative
            , Css.Global.descendants
                [ Css.Global.selector ".floating-label__input:not(:placeholder-shown) + label"
                    [ Css.backgroundColor <| Css.hex "ffffff"
                    , Css.transform <| Css.translate2 Css.zero (Css.pct -50)
                    , Css.opacity <| Css.num 1
                    ]
                ]
            ]
        ]
        [ Html.Styled.input
            [ Html.Styled.Attributes.class "floating-label__input"
            , Html.Styled.Attributes.placeholder stim
            , Html.Styled.Attributes.readonly givenIsFeedback
            , Html.Styled.Attributes.value val
            , Html.Styled.Attributes.css
                [ Css.padding <| Css.px 8
                , Css.borderRadius <| Css.px 4
                , Css.border3 (Css.px 1) Css.solid (Css.hex "efefef")
                ]
            , Html.Styled.Events.onInput msg
            ]
            []
        , Html.Styled.label
            [ Html.Styled.Attributes.class "floating-label__label"
            , Html.Styled.Attributes.css
                [ Css.position Css.absolute
                , Css.left <| Css.px 8
                , Css.top <| Css.px 0
                , Css.opacity Css.zero
                , Css.pointerEvents Css.none
                , Css.Transitions.transition
                    [ Css.Transitions.opacity 200
                    , Css.Transitions.transform 200
                    ]
                , Css.color (Css.hex "b6b6b6")
                ]
            ]
            [ Html.Styled.text stim ]
        ]


tooltip : String -> Html msg
tooltip text_ =
    Html.Styled.div
        [ Html.Styled.Attributes.css
            [ Css.position Css.relative
            , Css.hover
                [ Css.Global.descendants
                    [ Css.Global.selector ".tooltip__content"
                        [ Css.opacity <| Css.num 1
                        , Css.pointerEventsAll
                        ]
                    ]
                ]
            ]
        , Html.Styled.Attributes.class "h-12 w-12"
        ]
        [ Html.Styled.div
            [ Html.Styled.Attributes.class "tooltip__content"
            , Html.Styled.Attributes.css
                [ Css.opacity Css.zero
                , Css.position Css.absolute
                , Css.backgroundColor <| Css.hex "fffff"
                , Css.width <| Css.px 200
                , Css.color (Css.hex "333333")
                , Css.bottom <| Css.pct 100

                --, Css.right <| Css.pct 50
                --, Css.transform <| Css.translate2 (Css.pct -50) (Css.px -8)
                , Css.Transitions.transition
                    [ Css.Transitions.opacity 200 ]
                , Css.after
                    [ Css.property "content" " "
                    , Css.position Css.absolute
                    , Css.border3 (Css.px 8) Css.solid Css.transparent
                    , Css.borderTopColor <| Css.hex "333333"
                    , Css.bottom Css.zero

                    --, Css.left <| Css.pct 50
                    --, Css.transform <| Css.translate2 (Css.pct -50) (Css.px 16)
                    , Css.height Css.zero
                    , Css.width Css.zero
                    , Css.Transitions.transition
                        [ Css.Transitions.opacity 200 ]
                    ]
                ]
            ]
            [ div [ class "border-2 p-2 bg-white" ] [ text text_ ] ]
        , Html.Styled.div
            [ Html.Styled.Attributes.class "tooltip__trigger" ]
            [ div [ class "" ] [ Html.Styled.fromUnstyled Icons.helpCircle ] ]
        ]


sentenceInSynonym : { a | pre : String, stimulus : String, post : String } -> { b | userAnswer : String } -> (String -> msg) -> Bool -> Html msg
sentenceInSynonym t state msg feedback_ =
    div [ class "flex w-full border-2 p-4  space-x-4 text-xl text-center items-center" ]
        [ span [] [ text t.pre ]
        , floatingLabel t.stimulus state.userAnswer msg feedback_
        , span [] [ text t.post ]
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


simpleAudioPlayer : String -> Html msg
simpleAudioPlayer src =
    Html.Styled.audio
        [ Html.Styled.Attributes.controls True
        , Html.Styled.Attributes.src src
        , Html.Styled.Attributes.autoplay True
        , Html.Styled.Attributes.width 300
        ]
        []
