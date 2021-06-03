module Session3.Synonym exposing (..)

import Data exposing (decodeRecords)
import Dict
import ExperimentInfo
import Html.Styled exposing (Html, div, h4, p, span, text)
import Html.Styled.Attributes exposing (class)
import Http
import Json.Decode as Decode
import Json.Decode.Pipeline exposing (..)
import Logic
import Session1.ContextUnderstanding exposing (Msg(..))
import View


type Msg
    = UserClickedFeedback
    | UserChangedInput String
    | UserClickedNextTrial
    | UserClickedStartMainloop
    | SaveDataMsg
    | ServerRespondedWithLastRecords (Result Http.Error (List ()))
    | UserCLickedStartTraining


update msg model =
    case msg of
        UserClickedFeedback ->
            ( { model
                | synonymTask =
                    model.synonymTask
                        |> Logic.toggle
              }
            , Cmd.none
            )

        UserChangedInput newChoice ->
            ( { model
                | synonymTask =
                    model.synonymTask
                        |> Logic.update { uid = "", userAnswer = newChoice }
              }
            , Cmd.none
            )

        UserClickedNextTrial ->
            ( { model
                | synonymTask =
                    model.synonymTask |> Logic.next initState
              }
            , Cmd.none
            )

        SaveDataMsg ->
            let
                responseHandler =
                    ServerRespondedWithLastRecords
            in
            ( model, Logic.saveData responseHandler model.user taskId model.translationTask )

        UserClickedStartMainloop ->
            ( { model | synonymTask = Logic.startMain model.synonymTask initState }, Cmd.none )

        ServerRespondedWithLastRecords _ ->
            ( model, Cmd.none )

        UserCLickedStartTraining ->
            ( { model | synonymTask = Logic.startTraining model.synonymTask }, Cmd.none )


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
    Logic.Task Trial State
    -> List (Html Msg)
viewTask experiment =
    case experiment of
        Logic.Err reason ->
            [ h4 [] [ p [] [ text ("Failure" ++ reason) ] ]
            ]

        Logic.NotStarted ->
            [ text "I'm not started yet." ]

        Logic.Loading ->
            [ text "Loading..." ]

        Logic.Running Logic.Instructions data ->
            [ View.instructions data.infos.instructions UserCLickedStartTraining ]

        Logic.Running Logic.Training task ->
            let
                toggleFeedback =
                    View.button
                        { message = UserClickedFeedback
                        , txt = "Check my answer"
                        , isDisabled = False
                        }

                trainingBox =
                    div [ class "container w-full h-full border-4 border-green-500 border-rounded-lg border-dashed text-center object-center " ]
            in
            case ( task.current, task.feedback ) of
                ( Just x, False ) ->
                    [ trainingBox
                        [ trainingWheels (List.length task.history) x.radical x.target
                        , div [ class "p-8" ] [ View.sentenceInSynonym x task.state UserChangedInput task.feedback ]
                        , div [ class "m-8" ] [ toggleFeedback ]
                        ]
                    ]

                ( Just x, True ) ->
                    [ trainingBox
                        [ trainingWheels (List.length task.history) x.stimulus x.target
                        , div [ class "m-8" ] [ View.sentenceInSynonym x task.state UserChangedInput task.feedback ]
                        , div [ class " rounded-md text-center object-center bg-green-300 m-8" ]
                            [ p [ class "p-6 text-xl text-white" ]
                                [ text "The correct synonym for "
                                , text x.radical
                                , text " is "
                                , span [ class "font-bold" ] [ text x.target ]
                                ]
                            , div [ class "pb-4" ]
                                [ View.button
                                    { message = UserClickedNextTrial
                                    , txt = "Next"
                                    , isDisabled = False
                                    }
                                ]
                            ]
                        ]
                    ]

                ( Nothing, _ ) ->
                    [ View.introToMain <| UserClickedStartMainloop ]

        Logic.Running Logic.Main task ->
            case ( task.current, task.feedback ) of
                ( Just t, False ) ->
                    [ View.tooltip "Type the synonym of the word in the box"
                    , View.sentenceInSynonym t task.state UserChangedInput task.feedback
                    , View.button
                        { message = UserClickedFeedback
                        , txt = "Check my answer"
                        , isDisabled = False
                        }
                    ]

                -- TODO: Abstraire le feedback pour le partager entre la phase d'entrainement et la phase principale
                ( Just t, True ) ->
                    [ View.sentenceInSynonym t task.state UserChangedInput task.feedback
                    , div [ class "p-4" ] []
                    , div [ class "flex flex-col w-full rounded-lg h-48 bg-green-300 items-center text-center" ]
                        [ p [ class "pt-8 text-lg text-white" ] [ text <| "The best synonym for " ++ t.radical ++ " is ", span [ class "font-bold" ] [ text t.target ] ]
                        , View.button
                            { message = UserClickedNextTrial
                            , txt = "Next"
                            , isDisabled = False
                            }
                        ]
                    ]

                ( Nothing, _ ) ->
                    [ View.end task.infos.end SaveDataMsg "spelling" ]


start : List ExperimentInfo.Task -> List Trial -> Logic.Task Trial State
start info trials =
    let
        relatedInfos =
            Dict.get taskId (ExperimentInfo.toDict info) |> Result.fromMaybe ("I couldn't fetch the value associated with: " ++ taskId)
    in
    Logic.startIntro relatedInfos
        (List.filter (\datum -> datum.isTraining) trials)
        (List.filter (\datum -> not datum.isTraining) trials)
        initState


decodeSynonymTrials : Decode.Decoder (List Trial)
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
            Decode.succeed Trial
                |> required "UID" Decode.string
                |> required "Word_Text" Decode.string
                |> required "pre" Decode.string
                |> required "stim" Decode.string
                |> required "post" Decode.string
                |> optional "isTraining" Decode.bool False
                |> required "radical" Decode.string
    in
    decodeRecords decoder


initState : State
initState =
    State "DefaultUserUID" ""


defaultTrial : Trial
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
    "recB3kUQW4jNTlou6"


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


type alias Trial =
    { uid : String
    , target : String
    , pre : String
    , stimulus : String
    , post : String
    , isTraining : Bool
    , radical : String
    }


type alias State =
    { uid : String
    , userAnswer : String
    }
