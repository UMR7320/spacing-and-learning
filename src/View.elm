module View exposing (..)

import Css
import Css.Global
import Css.Transitions
import Html.Attributes as A
import Html.Styled exposing (..)
import Html.Styled.Attributes
    exposing
        ( attribute
        , checked
        , class
        , classList
        , href
        , id
        , name
        , placeholder
        , readonly
        , spellcheck
        , target
        , type_
        , value
        )
import Html.Styled.Events exposing (onClick, onInput)
import Icons
import Markdown
import String.Interpolate
import UserCode exposing (Msg)


shuffledOptions :
    { a | userAnswer : String }
    -> Bool
    -> (String -> msg)
    ->
        { c
            | target : String
            , distractor1 : String
            , distractor2 : String
            , distractor3 : String
        }
    -> List comparable
    -> Html msg
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
    div [ class "shuffled-options" ] (ordoredOptions ++ [ option "I don't know" ])


unclickableButton color children =
    div
        [ class ("flex items-center m-2 p-4 rounded-lg " ++ color ++ " justify-center") ]
        children


loading =
    div [] [ text "Loading... Please don't exit or data will be lost" ]


instructions infos msgToTraining =
    div [ class "instructions" ]
        [ div [ class "pb-8" ] [ fromMarkdown infos.instructions ]
        , button { message = msgToTraining, txt = "Continue", isDisabled = False }
        ]


{-| For instructions containing HTML that should not be escaped
-}
unsafeInstructions infos msgToTraining =
    div [ class "instructions" ]
        [ div [ class "pb-8" ] [ fromUnsafeMarkdown infos.instructions ]
        , button { message = msgToTraining, txt = "Continue", isDisabled = False }
        ]


unsafeInstructionsWithLink infos link =
    div [ class "instructions" ]
        [ div [ class "pb-8" ] [ fromUnsafeMarkdown infos.instructions ]
        , a
            [ href link
            , class "button"
            ]
            [ text "Continue" ]
        ]


end : String -> msg -> Maybe String -> Html msg
end endInfo saveDataMsg linkToNextTask =
    div [ class "endInfo" ]
        [ div []
            [ fromMarkdown endInfo
            , case linkToNextTask of
                Just url ->
                    div [ class "mt-8" ]
                        [ a [ href url ]
                            [ button
                                { message = saveDataMsg
                                , isDisabled = False
                                , txt = "Continue"
                                }
                            ]
                        ]

                Nothing ->
                    text ""
            ]
        ]


navigationButton toggleFeedbackMsg nextTrialMsg feedback userAnswer =
    button <|
        if not feedback then
            { message = toggleFeedbackMsg
            , txt = "Check my answer"
            , isDisabled = String.isEmpty userAnswer
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
        []
        [ if data.isVisible then
            div
                [ classList
                    [ ( "rounded-sm px-6 py-4 mt-2 mb-4", True )
                    , ( "bg-green-200", data.isVisible && data.userAnswer == data.target )
                    , ( "bg-red-200", data.isVisible && data.userAnswer /= data.target )
                    ]
                ]
                [ p
                    []
                    [ if data.userAnswer == data.target then
                        viewFeedback feedback_Correct

                      else
                        viewFeedback feedback_Incorrect
                    ]
                ]

          else
            text ""
        , data.button
        ]


viewFeedback : ( String, List String ) -> Html msg
viewFeedback ( feedback, args ) =
    args
        |> String.Interpolate.interpolate feedback
        |> fromMarkdown


genericNeutralFeedback : { isVisible : Bool, feedback_Correct : ( String, List String ), button : Html msg } -> Html msg
genericNeutralFeedback ({ feedback_Correct } as data) =
    div
        [ class
            ("w-full rounded-md "
                ++ (if data.isVisible then
                        "bg-indigo-800"

                    else if data.isVisible == False then
                        ""

                    else
                        ""
                   )
            )
        ]
        [ if data.isVisible then
            p
                [ class
                    "font-medium p-4 w-full text-white"
                ]
                [ fromMarkdown (String.Interpolate.interpolate (Tuple.first feedback_Correct) (Tuple.second feedback_Correct)) ]

          else
            div [] []
        , div [ class "p-4" ] [ data.button ]
        ]



--TRAINING


trainingBox : List (Html msg) -> Html msg
trainingBox =
    div [ class "flex flex-col items-center justify-center w-full h-full border-4 border-green-500 border-rounded-lg border-dashed " ]


viewInstructions : String -> Html msg
viewInstructions instructions_ =
    div [ class "flex flex-col" ]
        [ h2 [ class "font-bold" ] [ text "Instructions" ]
        , p [ class "pt-8 pb-8 font-medium" ]
            [ pre [] [ fromMarkdown instructions_ ]
            ]
        , div [ class "text-green-500 font-bold pb-2" ] [ span [] [ text "Practice here!" ] ]
        ]


viewTraining : String -> List (Html msg) -> Html msg
viewTraining instructions_ content =
    div [ class "flex flex-col" ]
        [ viewInstructions instructions_, trainingBox content ]


pct : Int -> List a -> Float
pct trialn trials =
    (toFloat trialn / toFloat (List.length trials)) * 100



-- HEADER


header : List (Html msg) -> Html msg
header items =
    nav
        [ class "border-b border-gray-300" ]
        [ div
            [ class "flex items-center px-6" ]
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


mainEl : List (Html msg) -> Html msg
mainEl content =
    main_ [ attribute "data-test" "content" ] content



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
        [ class
            ("text-gray-70 font-medium "
                ++ "border-solid border-2 px-4 py-3 mb-1 "
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


introToMain : msg -> Html msg
introToMain msg =
    div
        []
        [ p [ class "pt-16 mb-8" ] [ text "Now you understand the activity, let's try our target words." ]
        , button
            { message = msg
            , txt = "Start"
            , isDisabled = False
            }
        ]


def =
    Markdown.defaultOptions


fromMarkdown =
    fromUnstyled << Markdown.toHtml [ A.class "flow" ]


fromUnsafeMarkdown =
    fromUnstyled << Markdown.toHtmlWith { def | sanitize = False } [ A.class "flow" ]


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
            , Html.Styled.Attributes.class "placeholder-gray-600"
            , Html.Styled.Attributes.value val
            , Html.Styled.Attributes.css
                [ Css.padding <| Css.px 8
                , Css.borderRadius <| Css.px 4
                , Css.border3 (Css.px 1) Css.solid (Css.hex "#8a8d91")
                ]
            , Html.Styled.Events.onInput msg
            ]
            []
        , Html.Styled.label
            [ Html.Styled.Attributes.class "floating-label__label"
            , Html.Styled.Attributes.class "placeholder-gray-600"
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
    p [ class "sentence-in-synonym" ]
        [ span [] [ text t.pre ]
        , floatingLabel t.stimulus state.userAnswer msg feedback_
        , span [] [ text t.post ]
        ]


button : { message : msg, txt : String, isDisabled : Bool } -> Html msg
button { message, txt, isDisabled } =
    div [ class "flex justify-center" ]
        [ Html.Styled.button
            [ class "mt-2 mb-4"
            , onClick message
            , Html.Styled.Attributes.disabled isDisabled
            , class <|
                if isDisabled then
                    "invisible"

                else
                    "visible"
            ]
            [ text txt ]
        ]


audioButton msg url audioName =
    Html.Styled.button
        [ class "mt-2 mb-4 max-w-lg flex flex-row items-center justify-center bg-green-500 hover:bg-green-600 rounded-md"
        , Html.Styled.Events.onClick (msg url)
        ]
        [ div [ class "h-6 w-6 text-white" ] [ fromUnstyled <| Icons.music ], span [ class "pl-4 font-bold text-white" ] [ text <| audioName ] ]


readOnlyAmorce firstAmorce firstProduction =
    if String.length firstProduction < String.length firstAmorce then
        firstAmorce

    else
        firstProduction


readOnlyOnFeedback fb =
    if fb then
        readonly True

    else
        readonly False


textAreaWithReadonlyAmorce { id_, amorce, isFeedback, userAnswer, onInputMsg } =
    Html.Styled.textarea
        [ id id_
        , class "border-2 p-2 w-full max-w-3xl"
        , value <|
            readOnlyAmorce
                amorce
                userAnswer
        , onInput onInputMsg
        , spellcheck False
        , readOnlyOnFeedback isFeedback
        ]
        [ text amorce ]


textField { labelText, placeholderText, msg } =
    div [ class "flex flex-col" ]
        [ label [] [ text labelText ]
        , Html.Styled.input
            [ type_ "text"
            , class "border-2"
            , Html.Styled.Events.onInput msg
            , placeholder placeholderText
            ]
            []
        ]
