module Experiment.Synonym exposing (..)

import Array
import Data exposing (decodeRecords)
import Experiment.Experiment as E
import Html.Styled exposing (Html, div, fieldset, h1, h3, p, span, text)
import Html.Styled.Attributes exposing (class, disabled)
import Http
import Json.Decode as Decode
import Json.Decode.Pipeline exposing (..)
import Progressbar
import View


view : E.Experiment -> List (Html msg) -> msg -> msg -> Html msg
view exp options toggleFeedbackMsg nextTrialMsg =
    case exp of
        E.DoingSynonym (E.MainLoop trials state trialn isfeedback) ->
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
                        (if state.userAnswer == trial.target then
                            [ text "✔️ Correct Answer ! " ]

                         else
                            [ text "❌ The correct definition is : "
                            , span [ class "font-medium italic" ] [ text trial.target ]
                            ]
                        )

                viewQuestion =
                    h3 []
                        [ p []
                            [ text <| String.fromInt (trialn + 1) ++ ". " ++ "Choose the best definition for the word : "
                            , span [ class "italic" ] [ text trial.question ]
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

        _ ->
            div [] [ text "Todo : Intro, Puse ??" ]


decodeSynonymTrials : Decode.Decoder (List E.SynonymTrial)
decodeSynonymTrials =
    let
        decoder =
            Decode.succeed E.SynonymTrial
                |> required "UID" Decode.string
                |> required "Question_Synonym" Decode.string
                |> required "Word_Text" Decode.string
                |> required "Distractor_1_Synonym" Decode.string
                |> required "Distractor_2_Synonym" Decode.string
                |> required "Distractor_3_Synonym" Decode.string
    in
    decodeRecords decoder


getTrialsFromServer : (Result Http.Error (List E.SynonymTrial) -> msg) -> Cmd msg
getTrialsFromServer callbackMsg =
    E.getTrialsFromServer_ "Meaning" callbackMsg decodeSynonymTrials


initState : E.SynonymState
initState =
    E.SynonymState "DefaultTrialUID" "DefaultUserUID" ""


defaultTrial : E.SynonymTrial
defaultTrial =
    { uid = "uidMISSING"
    , question = "questionMISSING"
    , target = "targetMISSING"
    , distractor1 = "distractor1MISSING"
    , distractor2 = "distractor2MISSING"
    , distractor3 = "distractor3MISSING"
    }
