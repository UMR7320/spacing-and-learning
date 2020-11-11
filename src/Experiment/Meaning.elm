module Experiment.Meaning exposing (..)

import Array
import Browser
import Data exposing (decodeRecords)
import Experiment.Experiment as E exposing (..)
import Html.Styled exposing (Html, div, fieldset, h1, h2, h4, input, label, p, span, text)
import Html.Styled.Attributes exposing (checked, class, for, id, type_)
import Http
import Json.Decode as Decode exposing (Decoder, string)
import Json.Decode.Pipeline exposing (..)
import Url exposing (Url)
import View


view : Experiment Trial State -> List (Html msg) -> msg -> msg -> List (Html msg)
view exp options toggleFeedbackMsg nextTrialMsg =
    let
        trial =
            getTrial_ exp

        state =
            getState exp
    in
    case getStep exp of
        E.MainLoop _ showFeedback ->
            case showFeedback of
                False ->
                    [ h2 [] [ text <| trial.question ]
                    , div
                        [ class "py-10 max-w-xl" ]
                        [ fieldset
                            []
                            options
                        ]
                    , View.button toggleFeedbackMsg "Is it correct?"
                    ]

                True ->
                    [ h4 []
                        [ text <|
                            if state.userAnswer /= trial.definition then
                                trial.feedbackCorrect

                            else
                                trial.feedbackIncorrect
                        ]
                    , div
                        [ class "py-3 max-w-xl" ]
                        []
                    , View.button nextTrialMsg "Next"
                    ]

        E.End ->
            [ h1 [] [ text "Merci de votre participation !🎉" ]
            , p
                [ class "max-w-xl text-xl mb-8" ]
                [ text "Vous trouverez dans l'e-mail que vous avez reçu les liens pour la suite de l'expérience."
                ]
            ]

        _ ->
            [ text "Todo : Intro, Pause or Training" ]


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


getState : Experiment Trial State -> State
getState exp =
    case exp of
        Ready ( Meaning trials state, step ) ->
            state

        _ ->
            initState


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


updateState : State -> Experiment Trial State -> Experiment Trial State
updateState newState experiment =
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
    State "DefaultTrialUID" "DefaultUserUID" "DefaultUserAnswer"


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
