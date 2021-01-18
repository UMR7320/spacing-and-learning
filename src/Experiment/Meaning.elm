module Experiment.Meaning exposing (..)

import Array
import Browser
import Css exposing (visibility)
import Data exposing (decodeRecords)
import Experiment.Experiment as E
import Experiment.Synonym as Synonym
import Html.Styled
    exposing
        ( Html
        , div
        , fieldset
        , h1
        , h2
        , h3
        , h4
        , h5
        , h6
        , input
        , label
        , p
        , span
        , text
        )
import Html.Styled.Attributes exposing (checked, class, disabled, for, id, type_)
import Http
import Json.Decode as Decode exposing (Decoder, string)
import Json.Decode.Pipeline exposing (..)
import Progressbar
import Url exposing (Url)
import Url.Builder
import View


view : E.Experiment -> List (Html msg) -> msg -> msg -> Html msg
view exp options toggleFeedbackMsg nextTrialMsg =
    case exp of
        E.DoingMeaning (E.MainLoop trials state trialn isfeedback) ->
            let
                trial =
                    Array.get trialn (Array.fromList trials)
                        |> Maybe.withDefault defaultTrial

                pct =
                    (toFloat trialn / toFloat (List.length trials)) * 100

                viewFeedback isVisible =
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
                        (if state.userAnswer == trial.definition then
                            [ text "✔️ Correct Answer ! " ]

                         else
                            [ text "❌ The correct definition is : "
                            , span [ class "font-medium italic" ] [ text trial.definition ]
                            ]
                        )

                viewQuestion =
                    h3 []
                        [ p []
                            [ text <| String.fromInt (trialn + 1) ++ ". " ++ "Choose the best definition for the word : "
                            , span [ class "italic" ] [ text trial.writtenWord ]
                            ]
                        ]
            in
            div [ class "flex flex-wrap items-center" ]
                [ div [ class "mr-8" ]
                    [ Progressbar.progressBar pct
                    , viewQuestion
                    , div
                        [ class "pt-6 max-w-xl ", disabled isfeedback ]
                        [ fieldset
                            []
                            options
                        ]
                    , View.button <|
                        if not isfeedback then
                            { message = toggleFeedbackMsg
                            , txt = "Is it correct?"
                            , isDisabled = String.isEmpty state.userAnswer
                            }

                        else
                            { message = nextTrialMsg
                            , txt = "Next "
                            , isDisabled = False
                            }
                    , div [ class "mt-4 max-w-xl w-full" ] [ viewFeedback isfeedback ]
                    ]
                ]

        E.DoingMeaning (E.End txt) ->
            div []
                [ h1 [] [ text "Merci de votre participation !🎉" ]
                , p
                    [ class "max-w-xl text-xl mb-8" ]
                    [ text "Vous trouverez dans l'e-mail que vous avez reçu les liens pour la suite de l'expérience."
                    ]
                ]

        E.DoingTranslation (E.MainLoop trials state trialn feedback) ->
            let
                trial =
                    Array.get trialn (Array.fromList trials)
                        |> Maybe.withDefault E.defaultTranslationTrial

                pct =
                    (toFloat trialn / toFloat (List.length trials)) * 100

                viewFeedback isVisible =
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
                        (if state.userAnswer == trial.translation1 || state.userAnswer == trial.translation2 then
                            [ text "✔️ Correct Answer ! " ]

                         else
                            [ text "❌ The correct translation is : "
                            , span [ class "font-medium italic" ] [ text trial.translation1 ]
                            ]
                        )

                viewQuestion =
                    h3 []
                        [ p []
                            [ text <| String.fromInt (trialn + 1) ++ ". " ++ "Choose the best translation for the word: "
                            , span [ class "italic" ] [ text trial.word ]
                            ]
                        ]
            in
            div [ class "flex flex-wrap items-center" ]
                [ div [ class "mr-8" ]
                    [ Progressbar.progressBar pct
                    , viewQuestion
                    , div
                        [ class "pt-6 max-w-xl ", disabled feedback ]
                        [ fieldset
                            []
                            options
                        ]
                    , View.button <|
                        if not feedback then
                            { message = toggleFeedbackMsg
                            , txt = "Is it correct?"
                            , isDisabled = String.isEmpty state.userAnswer
                            }

                        else
                            { message = nextTrialMsg
                            , txt = "Next "
                            , isDisabled = False
                            }
                    , div [ class "mt-4 max-w-xl w-full" ] [ viewFeedback feedback ]
                    ]
                ]

        E.DoingSynonym (E.MainLoop trials state trialn feedback) ->
            let
                trial =
                    Array.get trialn (Array.fromList trials)
                        |> Maybe.withDefault Synonym.defaultTrial

                pct =
                    (toFloat trialn / toFloat (List.length trials)) * 100

                viewFeedback isVisible =
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
                            [ text "❌ The correct translation is : "
                            , span [ class "font-medium italic" ] [ text trial.target ]
                            ]
                        )

                viewQuestion =
                    h3 []
                        [ p []
                            [ text <| String.fromInt (trialn + 1) ++ ". " ++ "Choose the best synonym for the word: "
                            , span [ class "italic" ] [ text <| trial.pre ++ trial.stimulus ++ trial.post ]
                            ]
                        ]
            in
            div [ class "flex flex-wrap items-center" ]
                [ div [ class "mr-8" ]
                    [ Progressbar.progressBar pct
                    , viewQuestion
                    , div
                        [ class "pt-6 max-w-xl ", disabled feedback ]
                        [ fieldset
                            []
                            options
                        ]
                    , View.button <|
                        if not feedback then
                            { message = toggleFeedbackMsg
                            , txt = "Is it correct?"
                            , isDisabled = String.isEmpty state.userAnswer
                            }

                        else
                            { message = nextTrialMsg
                            , txt = "Next "
                            , isDisabled = False
                            }
                    , div [ class "mt-4 max-w-xl w-full" ] [ viewFeedback feedback ]
                    ]
                ]

        _ ->
            div [] [ text "Todo : Intro, Pause ??" ]
--}


getTrial : Int -> List input -> input -> input
getTrial trial trials_ default =
    let
        trials =
            Array.fromList trials_
    in
    case Array.get trial trials of
        Nothing ->
            default

        Just x ->
            x


decodeMeaningInput : Decoder (List E.TrialMeaning)
decodeMeaningInput =
    let
        decoder =
            Decode.succeed E.TrialMeaning
                |> required "UID" string
                |> required "Word_Text" string
                |> required "Definition" string
                |> required "Question_Meaning" string
                |> optional "Distractor_1_Meaning" string "MISSING"
                |> optional "Distractor_2_Meaning" string "MISSING"
                |> optional "Distractor_3_Meaning" string "MISSING"
                |> optional "Distractor_4_Meaning" string "Missing"
                |> required "Feedback_Incorrect_Meaning" string
                |> required "Feedback_Correct_Meaning" string
    in
    decodeRecords decoder


getTrialsFromServer : (Result Http.Error (List E.TrialMeaning) -> msg) -> Cmd msg
getTrialsFromServer callbackMsg =
    E.getTrialsFromServer_ "Meaning" callbackMsg decodeMeaningInput


initState : E.StateMeaning
initState =
    E.StateMeaning "DefaultTrialUID" "DefaultUserUID" ""


defaultTrial : E.TrialMeaning
defaultTrial =
    { uid = "MISSING"
    , writtenWord = "MISSING"
    , definition = "MISSING"
    , question = "MISSING"
    , option1 = "MISSING"
    , option2 = "MISSING"
    , option3 = "MISSING"
    , option4 = "Default"
    , feedbackCorrect = "MISSING"
    , feedbackIncorrect = "MISSING"
    }
