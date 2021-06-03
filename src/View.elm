module View exposing
    ( audioButton
    , bold
    , button
    , container
    , end
    , floatingLabel
    , fromMarkdown
    , genericNeutralFeedback
    , genericSingleChoiceFeedback
    , header
    , instructions
    , introToMain
    , loading
    , navOut
    , navigationButton
    , notFound
    , pct
    , radio
    , sentenceInSynonym
    , shuffledOptions
    , tooltip
    , trainingWheelsGeneric
    , unclickableButton
    , viewTraining
    )

import Css
import Css.Global
import Css.Transitions
import Html.Styled exposing (..)
import Html.Styled.Attributes
    exposing
        ( attribute
        , checked
        , class
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
import String.Interpolate exposing (interpolate)


shuffledOptions :
    { a | userAnswer : String }
    -> Bool
    -> (String -> b)
    ->
        { c
            | target : String
            , distractor1 : String
            , distractor2 : String
            , distractor3 : String
        }
    -> List comparable
    -> List (Html b)
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
    ordoredOptions ++ [ option "I don't know" ]


unclickableButton color txt =
    div [ class <| "flex flex-col items-center m-2 p-4 rounded-lg " ++ color ++ " justify-center" ] [ text txt ]


loading =
    div [] [ text "Loading... Please don't quit or data will be lost" ]


instructions content msgToTraining =
    div [ class "flex text-lg flex-col items-center" ]
        [ h1 [] [ text "Instructions" ]
        , fromMarkdown content
        , button { message = msgToTraining, txt = "Continue", isDisabled = False }
        ]


end endInfo saveDataMsg linkToNextTask =
    div [ class "flex flex-col text-lg w-full items-center" ]
        [ fromMarkdown endInfo
        , a [ href linkToNextTask ]
            [ button
                { message = saveDataMsg
                , isDisabled = False
                , txt = "Click here when you are ready !"
                }
            ]
        ]


navigationButton toggleFeedbackMsg nextTrialMsg feedback =
    button <|
        if not feedback then
            { message = toggleFeedbackMsg
            , txt = "Check my answer"
            , isDisabled = False
            }

        else
            { message = nextTrialMsg
            , txt = "Continue"
            , isDisabled = False
            }


bold string =
    "**" ++ string ++ "**"


genericSingleChoiceFeedback :
    { isVisible : Bool
    , userAnswer : String
    , target : String
    , feedback_Correct : ( String, List String )
    , feedback_Incorrect : ( String, List String )
    , button : Html msg
    }
    -> Html msg
genericSingleChoiceFeedback ({ feedback_Correct, feedback_Incorrect } as data) =
    div
        [ class
            (" w-full max-w-2xl rounded-md text-center object-center "
                ++ (if data.isVisible && data.userAnswer == data.target then
                        "bg-green-700"

                    else if data.isVisible && data.userAnswer /= data.target then
                        "bg-red-700"

                    else if data.isVisible == False then
                        ""

                    else
                        ""
                   )
            )
        ]
        [ p
            [ class
                ("text-lg py-4 w-full flex flex-col items-center justify-center  text-white p-2"
                    ++ " "
                    ++ (if data.isVisible then
                            "visible"

                        else
                            "invisible"
                       )
                )
            ]
            (if data.userAnswer == data.target then
                [ div [ class "w-12 h-12" ] [ fromUnstyled Icons.checkCircle ], fromMarkdown <| String.Interpolate.interpolate (Tuple.first feedback_Correct) (Tuple.second feedback_Correct) ]

             else
                [ div [ class "w-12 h-12" ] [ fromUnstyled Icons.xCircle ]
                , fromMarkdown <| String.Interpolate.interpolate (Tuple.first feedback_Incorrect) (Tuple.second feedback_Incorrect)
                ]
            )
        , div [ class "p-4" ] [ data.button ]
        ]


genericNeutralFeedback : { isVisible : Bool, feedback_Correct : ( String, List String ), button : Html msg } -> Html msg
genericNeutralFeedback ({ feedback_Correct } as data) =
    div
        [ class
            ("max-w-xl w-full rounded-md text-center object-center  mb-8 "
                ++ (if data.isVisible then
                        "bg-indigo-800"

                    else if data.isVisible == False then
                        ""

                    else
                        ""
                   )
            )
        ]
        [ p
            [ class
                ("font-medium py-4 w-full text-white"
                    ++ " "
                    ++ (if data.isVisible then
                            "visible"

                        else
                            "invisible"
                       )
                )
            ]
            [ fromMarkdown (String.Interpolate.interpolate (Tuple.first feedback_Correct) (Tuple.second feedback_Correct)) ]
        , div [ class "p-4" ] [ data.button ]
        ]



--TRAINING


trainingBox : List (Html msg) -> Html msg
trainingBox =
    div [ class "container flex flex-col items-center justify-center w-full h-full border-4 border-green-500 border-rounded-lg border-dashed " ]


viewInstructions : String -> Html msg
viewInstructions instructions_ =
    div [ class "flex flex-col" ]
        [ h2 [ class "font-bold" ] [ text "Instructions" ]
        , p [ class "pt-8 pb-8 font-medium" ]
            [ pre [] [ fromMarkdown instructions_ ]
            ]
        , div [ class "text-lg text-green-500 font-bold pb-2" ] [ span [] [ text "Practice here !" ] ]
        ]


trainingWheelsGeneric : Int -> String -> List String -> Html msg
trainingWheelsGeneric trialn pattern_ variables =
    let
        helpSentence =
            div [ class "flex flex-col pt-4 pb-4 italic text-lg max-w-xl" ] [ p [] [ fromMarkdown <| interpolate pattern_ variables ] ]
    in
    case trialn of
        0 ->
            helpSentence

        _ ->
            div [] []


viewTraining : String -> List (Html msg) -> Html msg
viewTraining instructions_ content =
    div [ class "flex flex-col" ]
        [ viewInstructions instructions_, trainingBox content ]


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
        [ class "fixed z-7 top-0 inset-x-0 bg-white border-b border-gray-300" -- Tailwind utilities: https://tailwindcss.com
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
                    []
                    [ text "👩\u{200D}🎓️" ]
                , p
                    [ class "font-bold uppercase text-sm text-gray-800" ]
                    [ text "Lex Learn" ]
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


radio : String -> Bool -> Bool -> Bool -> msg -> Html msg
radio value isChecked isCorrect feedbackMode msg =
    label
        [ class "group block text-gray-70 font-medium ", id value ]
        [ div
            [ class
                ("border-solid border-2 px-4 py-4 mb-1 "
                    ++ (case ( feedbackMode, isChecked, isCorrect ) of
                            ( True, True, False ) ->
                                "border-red-500"

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
                , class "form-radio text-indigo-500"
                , onClick msg
                , Html.Styled.Attributes.disabled feedbackMode
                ]
                []
            , span [ class "pl-4 " ] [ text value ]
            ]
        ]


introToMain : msg -> Html msg
introToMain msg =
    div [ class "container flex flex-col text-lg w-full items-center justify-center" ]
        [ h3 [] [ text "Now you understand the activity, let's try our target words." ]
        , button
            { message = msg
            , txt = "Start"
            , isDisabled = False
            }
        ]


def =
    Markdown.defaultOptions


fromMarkdown =
    fromUnstyled << Markdown.toHtmlWith { def | sanitize = False } []


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
        , Html.Styled.Attributes.class "h-6 w-6"
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
            [ Html.Styled.Attributes.class "tooltip__trigger opacity-75" ]
            [ div [ class "" ] [ Html.Styled.fromUnstyled Icons.helpCircle ] ]
        ]


sentenceInSynonym : { a | pre : String, stimulus : String, post : String } -> { b | userAnswer : String } -> (String -> msg) -> Bool -> Html msg
sentenceInSynonym t state msg feedback_ =
    div [ class "flex w-full border-2 p-4  space-x-4 text-lg text-center items-center" ]
        [ span [] [ text t.pre ]
        , floatingLabel t.stimulus state.userAnswer msg feedback_
        , span [] [ text t.post ]
        ]


button : { message : msg, txt : String, isDisabled : Bool } -> Html msg
button { message, txt, isDisabled } =
    Html.Styled.button
        [ class "max-w-xl w-full mt-6 mb-4"
        , onClick message
        , Html.Styled.Attributes.disabled isDisabled
        , class <|
            if isDisabled then
                "invisible"

            else
                "visible"
        ]
        [ text txt ]


audioButton msg url audioName =
    div
        [ class "px-4 py-4 mt-4 mb-4 hover:opacity-75 cursor-pointer border-2 flex flex-row items-center justify-center bg-green-500 rounded-md shadow-sm"
        , Html.Styled.Events.onClick (msg url)
        ]
        [ div [ class "h-6 w-6 text-white" ] [ fromUnstyled <| Icons.music ], span [ class "pl-4 text-lg font-bold text-white" ] [ text <| "Listen to the " ++ audioName ] ]
