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
import Progressbar
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
            [ View.loading ]

        Logic.Running Logic.Instructions data ->
            [ View.instructions data.infos.instructions UserCLickedStartTraining ]

        Logic.Running Logic.Training task ->
            case task.current of
                Just x ->
                    [ styledDiv
                        [ div [ class "p-8" ] [ View.sentenceInSynonym x task.state UserChangedInput task.feedback ]
                        , View.genericNeutralFeedback
                            { isVisible = task.feedback
                            , feedback_Correct = ( task.infos.feedback_correct, [ x.radical, x.target ] )
                            , button = View.navigationButton UserClickedFeedback UserClickedNextTrial task.feedback task.state.userAnswer
                            }
                        ]
                    ]

                Nothing ->
                    [ View.introToMain <| UserClickedStartMainloop ]

        Logic.Running Logic.Main task ->
            case task.current of
                Just x ->
                    [ styledDiv
                        [ View.tooltip "Type the synonym of the word in the box"
                        , Progressbar.progressBar task.history task.mainTrials
                        , div [] [ View.sentenceInSynonym x task.state UserChangedInput task.feedback ]
                        , View.genericNeutralFeedback
                            { isVisible = task.feedback
                            , feedback_Correct = ( task.infos.feedback_correct, [ x.radical, x.target ] )
                            , button = View.navigationButton UserClickedFeedback UserClickedNextTrial task.feedback task.state.userAnswer
                            }
                        ]
                    ]

                Nothing ->
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


styledDiv =
    div [ class "flex flex-col items-center w-full h-full items-center  text-center object-center " ]


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
