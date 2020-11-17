module Experiment.Meaning exposing (..)

import Array
import Browser
import Css exposing (visibility)
import Data exposing (decodeRecords)
import Experiment.Experiment as E exposing (..)
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


view : Experiment Trial State -> List (Html msg) -> msg -> msg -> Html msg
view exp options toggleFeedbackMsg nextTrialMsg =
    let
        trial =
            getTrial_ exp

        trials =
            getTrials exp

        trialN =
            getTrialNumber exp

        pct =
            (toFloat trialN / toFloat (List.length trials)) * 100

        state =
            getState exp |> Maybe.withDefault initState

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
                    [ text <| String.fromInt (trialN + 1) ++ ". " ++ "Choose the best definition for the word : "
                    , span [ class "italic" ] [ text trial.writtenWord ]
                    ]
                ]
    in
    case getStep exp of
        E.MainLoop _ showFeedback ->
            div [ class "flex flex-wrap items-center" ]
                [ div [ class "mr-8" ]
                    [ Progressbar.progressBar pct
                    , viewQuestion
                    , div
                        [ class "pt-6 max-w-xl ", disabled showFeedback ]
                        [ fieldset
                            []
                            options
                        ]
                    , View.button <|
                        if not showFeedback then
                            { message = toggleFeedbackMsg
                            , txt = "Is it correct?"
                            , isDisabled = String.isEmpty state.userAnswer
                            }

                        else
                            { message = nextTrialMsg
                            , txt = "Next "
                            , isDisabled = False
                            }
                    , div [ class "mt-4 max-w-xl w-full" ] [ viewFeedback showFeedback ]
                    ]
                ]

        E.End ->
            div []
                [ h1 [] [ text "Merci de votre participation !🎉" ]
                , p
                    [ class "max-w-xl text-xl mb-8" ]
                    [ text "Vous trouverez dans l'e-mail que vous avez reçu les liens pour la suite de l'expérience."
                    ]
                ]

        _ ->
            div [] [ text "Todo : Intro, Pause or Training" ]


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



{--
getState : Experiment Trial State -> State
getState exp =
    case exp of
        Ready ( Meaning trials state, step ) ->
            state

        _ ->
            initState
--}


getTrial_ : Experiment Trial State -> Trial
getTrial_ exp =
    case exp of
        Ready ( Meaning trials state, MainLoop ntrial _ ) ->
            case Array.get ntrial (Array.fromList trials) of
                Just x ->
                    x

                _ ->
                    defaultTrial

        _ ->
            defaultTrial


getStep : Experiment Trial State -> Step
getStep exp =
    case exp of
        Ready ( Meaning trials state, step ) ->
            step

        _ ->
            End


getTrialNumber : Experiment Trial State -> Int
getTrialNumber exp =
    case exp of
        Ready ( _, E.MainLoop trialN _ ) ->
            trialN

        _ ->
            0


getTrials : Experiment Trial State -> List Trial
getTrials exp =
    case exp of
        Ready ( Meaning trials state, _ ) ->
            trials

        _ ->
            []


getFeedbackStatus : Step -> Bool
getFeedbackStatus step =
    case step of
        MainLoop _ feedback ->
            feedback

        _ ->
            False


updateState : State -> Experiment Trial State -> Experiment Trial State
updateState newState experiment =
    case experiment of
        Ready ( Meaning trials state, step ) ->
            Ready ( Meaning trials newState, step )

        _ ->
            Failure (Http.BadBody "I tried to update the state of the 'Meaning' part of the experiment but I ran into an unexpected case.")


updateState_ : State -> Experiment Trial State -> Experiment Trial State
updateState_ newState experiment =
    case experiment of
        Ready ( Meaning trials state, step ) ->
            Ready ( Meaning trials newState, step )

        _ ->
            Failure (Http.BadBody "I tried to update the state of the 'Meaning' part of the experiment but I ran into an unexpected case.")


decodeMeaningInput : Decoder (List Trial)
decodeMeaningInput =
    let
        decoder =
            Decode.succeed Trial
                |> required "UID" string
                |> required "Word_Text" string
                |> required "Definition" string
                |> required "Question_Meaning" string
                |> optional "Distractor_1_Meaning" string "MISSING"
                |> optional "Distractor_2_Meaning" string "MISSING"
                |> optional "Distractor_3_Meaning" string "MISSING"
                |> required "Feedback_Incorrect_Meaning" string
                |> required "Feedback_Correct_Meaning" string
    in
    decodeRecords decoder


getTrialsFromServer : (Result Http.Error (List Trial) -> msg) -> Cmd msg
getTrialsFromServer callbackMsg =
    E.getTrialsFromServer_ "Meaning" callbackMsg decodeMeaningInput


type alias Trial =
    { uid : String
    , writtenWord : String
    , definition : String
    , question : String
    , option1 : String
    , option2 : String
    , option3 : String
    , feedbackCorrect : String
    , feedbackIncorrect : String
    }


type alias State =
    { inputUid : String
    , userUID : String
    , userAnswer : String
    }


initState : State
initState =
    State "DefaultTrialUID" "DefaultUserUID" ""


defaultTrial : Trial
defaultTrial =
    { uid = "MISSING"
    , writtenWord = "MISSING"
    , definition = "MISSING"
    , question = "MISSING"
    , option1 = "MISSING"
    , option2 = "MISSING"
    , option3 = "MISSING"
    , feedbackCorrect = "MISSING"
    , feedbackIncorrect = "MISSING"
    }
