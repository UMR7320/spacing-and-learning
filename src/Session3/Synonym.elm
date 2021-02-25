module Session3.Synonym exposing (..)

import Array
import Data exposing (buildErrorMessage, decodeRecords)
import Experiment.Experiment as E
import ExperimentInfo
import Html.Styled exposing (Html, div, fieldset, h1, h2, h3, h4, p, pre, span, text)
import Html.Styled.Attributes exposing (class, disabled)
import Http
import Icons
import Json.Decode as Decode
import Json.Decode.Pipeline exposing (..)
import Progressbar
import View



{--Route.Synonym ->
                    ( { model
                        | synonymTask =
                            model.synonymTask
                                |> E.toggleFeedback
                                |> E.updateState (E.SynonymStateType { currentSynonymState | userAnswer = "" })
                                |> E.nextTrial
                      }
                    , Cmd.none
                    )--}


type Msg
    = UserClickedFeedback
    | UserChangedInput String
    | UserClickedNextTrial
    | UserClickedStartMainloop (List E.SynonymTrial)
    | UserValidatedInput


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


viewTask :
    E.Experiment
    -> Maybe ExperimentInfo.Task
    ->
        { toggleFeedbackMsg : msg
        , updateInputMsg : String -> msg
        , nextTrialMsg : msg
        , toMainloopMsg : List E.SynonymTrial -> msg
        , inputValidationMsg : msg
        }
    -> List (Html msg)
viewTask experiment maybeInfos { toggleFeedbackMsg, updateInputMsg, nextTrialMsg, toMainloopMsg, inputValidationMsg } =
    case ( experiment, maybeInfos ) of
        ( E.Failure reason, _ ) ->
            [ h4 [] [ p [] [ text ("Failure" ++ buildErrorMessage reason) ] ]
            ]

        ( E.DoingSynonym (E.Intro trials state trialn feedback instructions), Just infos ) ->
            let
                trainingTrial =
                    trials
                        |> List.filter (\datum -> datum.isTraining)
                        |> Array.fromList
                        |> Array.get trialn

                toggleFeedback =
                    View.button
                        { message = toggleFeedbackMsg
                        , txt = "Check my answer"
                        , isDisabled = False
                        }

                viewInstructions x =
                    div [ class "flex flex-col" ]
                        [ h2 [ class "font-bold" ] [ text "Instructions" ]
                        , p [ class "pt-8 pb-8 font-medium" ]
                            [ pre [] [ text infos.instructions ]
                            ]
                        , div [ class "text-lg text-green-500 font-bold pb-2" ] [ span [] [ text "Practice here !" ] ]
                        ]

                trainingBox =
                    div [ class "container w-full h-full border-4 border-green-500 border-rounded-lg border-dashed text-center object-center " ]
            in
            case ( trainingTrial, feedback ) of
                ( Just x, False ) ->
                    [ viewInstructions x
                    , trainingBox
                        [ trainingWheels trialn x.radical x.target
                        , div [ class "p-8" ] [ View.sentenceInSynonym x state updateInputMsg feedback ]
                        , div [ class "m-8" ] [ toggleFeedback ]
                        ]
                    ]

                ( Just x, True ) ->
                    [ viewInstructions x
                    , trainingBox
                        [ trainingWheels trialn x.stimulus x.target
                        , div [ class "m-8" ] [ View.sentenceInSynonym x state updateInputMsg feedback ]
                        , div [ class " rounded-md text-center object-center bg-green-300 m-8" ]
                            [ p [ class "p-6 text-xl text-white" ]
                                [ text "The correct synonym for "
                                , text x.radical
                                , text " is "
                                , span [ class "font-bold" ] [ text x.target ]
                                ]
                            , div [ class "pb-4" ]
                                [ View.button
                                    { message = nextTrialMsg
                                    , txt = "Next"
                                    , isDisabled = False
                                    }
                                ]
                            ]
                        ]
                    ]

                ( Nothing, _ ) ->
                    [ div [ class "flex flex-col" ]
                        [ text "Now you understand the activity, let's try our target words."
                        , View.button
                            { message = toMainloopMsg trials
                            , txt = "Start"
                            , isDisabled = False
                            }
                        ]
                    ]

        ( E.DoingSynonym (E.MainLoop trials state trialn feedback), Just infos ) ->
            let
                trial =
                    trials
                        |> List.filter (\datum -> not datum.isTraining)
                        |> Array.fromList
                        |> Array.get trialn
            in
            case ( trial, feedback ) of
                ( Just t, False ) ->
                    [ View.tooltip "Type the synonym of the word in the box"
                    , View.sentenceInSynonym t state updateInputMsg feedback
                    , View.button
                        { message = inputValidationMsg
                        , txt = "Check my answer"
                        , isDisabled = False
                        }
                    ]

                -- TODO: Abstraire le feedback pour le partager entre la phase d'entrainement et la phase principale
                ( Just t, True ) ->
                    [ View.sentenceInSynonym t state updateInputMsg feedback
                    , div [ class "p-4" ] []
                    , div [ class "flex flex-col w-full rounded-lg h-48 bg-green-300 items-center text-center" ]
                        [ p [ class "pt-8 text-lg text-white" ] [ text <| "The best synonym for " ++ t.radical ++ " is ", span [ class "font-bold" ] [ text t.target ] ]
                        , View.button
                            { message = nextTrialMsg
                            , txt = "Next"
                            , isDisabled = False
                            }
                        ]
                    ]

                ( Nothing, _ ) ->
                    [ text "You've completed the meaning activity. Now let's concentrate on the form." ]

        ( E.DoingSynonym (E.End txt), _ ) ->
            [ text "Synonym est fini" ]

        ( _, Nothing ) ->
            [ text "I can't find the requested task's infos. Please report this issue." ]

        _ ->
            [ text "Unexpected view. You can take it in account in Main.viewExperiment" ]


decodeSynonymTrials : Decode.Decoder (List E.SynonymTrial)
decodeSynonymTrials =
    let
        stringToBoolDecoder : String -> Decode.Decoder Bool
        stringToBoolDecoder str =
            case str of
                "true" ->
                    Decode.succeed True

                _ ->
                    Decode.succeed False

        decoder =
            Decode.succeed E.SynonymTrial
                |> required "UID" Decode.string
                |> required "Word_Text" Decode.string
                |> required "pre" Decode.string
                |> required "stim" Decode.string
                |> required "post" Decode.string
                |> custom (Decode.field "isTraining" Decode.string |> Decode.andThen stringToBoolDecoder)
                |> required "radical" Decode.string
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
    , target = "targetMISSING"
    , pre = "preMissing"
    , stimulus = "stimulusMissing"
    , post = "postMissing"
    , isTraining = False
    , radical = "defaultRadical"
    }


taskId =
    "recf5HANE632FLKbc"


getRecords =
    Http.task
        { method = "GET"
        , headers = []
        , url =
            Data.buildQuery
                { app = Data.apps.spacing
                , base = "input"
                , view_ = "Presentation"
                }
        , body = Http.emptyBody
        , resolver = Http.stringResolver <| Data.handleJsonResponse <| decodeSynonymTrials
        , timeout = Just 5000
        }
