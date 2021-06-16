module Pretest.VKS exposing (..)

import Data
import Dict
import ExperimentInfo
import Html.Attributes exposing (placeholder)
import Html.Styled exposing (..)
import Html.Styled.Attributes as A exposing (class, type_)
import Html.Styled.Events as E
import Http
import Json.Decode as Decode
import Json.Decode.Pipeline exposing (optional, required)
import Json.Encode as Encode
import Logic
import Pretest.Acceptability exposing (ErrorBlock(..))
import Pretest.SPR exposing (Msg(..))
import Progressbar exposing (progressBarWhenNoTraining)
import Random
import Task
import View


taskId =
    "recR6grI83e1so6Zl"


type alias SC =
    Logic.Task Trial State


view : SC -> List (Html Msg)
view task =
    case task of
        Logic.Running Logic.Training data ->
            case data.current of
                Just _ ->
                    [ text "vks"
                    ]

                Nothing ->
                    [ div [ A.class "flex flex-col items-center" ]
                        [ View.fromMarkdown data.infos.introToMain
                        , View.button
                            { message = UserClickedStartMain
                            , txt = "Start"
                            , isDisabled = False
                            }
                        ]
                    ]

        Logic.Running Logic.Main data ->
            case data.current of
                Just trial ->
                    [ div [ A.class "flex flex-col items-center" ]
                        [ View.tooltip data.infos.instructions_short
                        , progressBarWhenNoTraining data.history data.mainTrials
                        , span [ class "text-lg font-bold" ] [ text <| "to " ++ trial.verb ]
                        , Html.Styled.fieldset [ class "flex flex-col m-2" ]
                            [ Html.Styled.label []
                                [ Html.Styled.input
                                    [ type_ "radio"
                                    , A.id "ns"
                                    , A.value "NeverSeen"
                                    , A.checked (data.state.knowledge == NeverSeen)
                                    , E.onInput UserClickedNewKnowledge
                                    ]
                                    []
                                , span [ class "p-2" ] [ text "I don’t remember having seen this verb before" ]
                                ]
                            , Html.Styled.label []
                                [ Html.Styled.input
                                    [ type_ "radio"
                                    , A.value "PreviouslySeen"
                                    , A.checked (data.state.knowledge == PreviouslySeen)
                                    , E.onInput UserClickedNewKnowledge
                                    ]
                                    []
                                , span [ class "p-2" ] [ text "I have seen this verb before, but I don’t know what it means" ]
                                ]
                            , Html.Styled.label []
                                [ Html.Styled.input
                                    [ type_ "radio"
                                    , A.value "Known"
                                    , A.checked (data.state.knowledge == Known)
                                    , E.onInput UserClickedNewKnowledge
                                    ]
                                    []
                                , span [ class "p-2" ] [ text "I have seen this verb before, and I think I know what it means" ]
                                ]
                            ]
                        , if data.state.knowledge == Known then
                            Html.Styled.fieldset [ class "flex flex-col p-2" ]
                                [ label [ class "flex flex-col" ]
                                    [ text "What do you think this verb means?"
                                    ]
                                , Dict.map
                                    (\k v ->
                                        div [ class "flex flex-row items-center" ]
                                            [ input
                                                [ type_ "text"
                                                , class "border-2 mt-2 w-full"
                                                , E.onInput (UserUpdatedField FirstProduction k)
                                                , A.placeholder "Translation OR Synonym OR definition"
                                                ]
                                                []
                                            , div [ E.onClick (UserClickedRemoveAnswer k), class "cursor-pointer mt-2 pl-2" ] [ text "❌" ]
                                            ]
                                    )
                                    data.state.definition
                                    |> Dict.values
                                    |> div []
                                , div [ E.onClick UserClickedAddAnswer, class "cursor-pointer font-bold hover:underline pt-2" ] [ text "➕ New answer" ]
                                , label [ class "flex flex-col p-2" ]
                                    [ text "Please use this verb in a sentence. The sentence should show that you know what the word means."
                                    , textarea [ class "border-2", E.onInput (UserUpdatedField SecondProduction 1) ] []
                                    ]
                                ]

                          else
                            text ""
                        , View.button
                            { txt = "Next Item"
                            , message = UserClickedNextTrial
                            , isDisabled =
                                if data.state.knowledge == Known && List.any String.isEmpty ([ data.state.usage ] ++ (data.state.definition |> Dict.values)) then
                                    True

                                else if data.state.knowledge == NoAnswer then
                                    True

                                else
                                    False
                            }
                        ]
                    ]

                Nothing ->
                    [ View.end data.infos.end UserClickedSaveData "" ]

        Logic.Err reason ->
            [ text reason ]

        Logic.Loading ->
            [ text "Loading... Please don't quit or data may be lost" ]

        Logic.NotStarted ->
            [ text "C'est tout bon!" ]

        Logic.Running Logic.Instructions data ->
            [ View.instructions data.infos.instructions UserClickedStartMain ]


getRecords =
    Http.task
        { method = "GET"
        , headers = []
        , url =
            Data.buildQuery
                { app = Data.apps.spacing
                , base = "input"
                , view_ = "Meaning"
                }
        , body = Http.emptyBody
        , resolver = Http.stringResolver <| Data.handleJsonResponse <| decodeAcceptabilityTrials
        , timeout = Just 5000
        }


decodeAcceptabilityTrials : Decode.Decoder (List Trial)
decodeAcceptabilityTrials =
    let
        decoder =
            Decode.succeed Trial
                |> required "id" Decode.string
                |> required "Word_Text" Decode.string
                |> optional "isTraining" Decode.bool False
    in
    Data.decodeRecords decoder


type alias Trial =
    { id : String
    , verb : String
    , isTraining : Bool
    }


type Familiarity
    = NeverSeen
    | PreviouslySeen
    | Known
    | NoAnswer


type alias State =
    { knowledge : Familiarity
    , definition : Dict.Dict Int String
    , usage : String
    }


type Msg
    = UserClickedNextTrial
    | UserClickedStartMain
    | UserClickedSaveData
    | ServerRespondedWithLastRecords (Result.Result Http.Error (List ()))
    | UserUpdatedField Field Int String
    | RuntimeReordedAmorces Field
    | UserClickedNewKnowledge String
    | UserClickedAddAnswer
    | UserClickedRemoveAnswer Int


type Field
    = FirstProduction
    | SecondProduction


type alias Model superModel =
    { superModel | vks : Logic.Task Trial State, user : Maybe String }


update : Msg -> Model superModel -> ( Model superModel, Cmd Msg )
update msg model =
    let
        prevState =
            Logic.getState model.vks |> Maybe.withDefault initState
    in
    case msg of
        RuntimeReordedAmorces _ ->
            ( { model | vks = model.vks |> Logic.update prevState }, Cmd.none )

        UserClickedNextTrial ->
            ( { model | vks = model.vks |> Logic.toggle |> Logic.next initState }, Random.generate RuntimeReordedAmorces (Random.uniform FirstProduction [ SecondProduction ]) )

        UserClickedStartMain ->
            ( { model | vks = Logic.startMain model.vks initState }, Cmd.none )

        UserUpdatedField fieldId subKey new ->
            case fieldId of
                FirstProduction ->
                    ( { model | vks = model.vks |> Logic.update { prevState | definition = Dict.insert subKey new prevState.definition } }, Cmd.none )

                SecondProduction ->
                    ( { model | vks = model.vks |> Logic.update { prevState | usage = new } }, Cmd.none )

        UserClickedSaveData ->
            let
                responseHandler =
                    ServerRespondedWithLastRecords
            in
            ( { model | vks = Logic.Loading }, saveData responseHandler model.user model.vks )

        ServerRespondedWithLastRecords (Result.Ok _) ->
            ( { model | vks = Logic.NotStarted }, Cmd.none )

        ServerRespondedWithLastRecords (Result.Err reason) ->
            ( { model | vks = Logic.Err (Data.buildErrorMessage reason) }, Cmd.none )

        UserClickedNewKnowledge str ->
            ( { model | vks = model.vks |> Logic.update { prevState | knowledge = familiarityFromString str } }, Cmd.none )

        UserClickedAddAnswer ->
            let
                lastIndex =
                    Dict.size prevState.definition
            in
            ( { model | vks = model.vks |> Logic.update { prevState | definition = Dict.insert (lastIndex + 1) "" prevState.definition } }, Cmd.none )

        UserClickedRemoveAnswer answerid ->
            ( { model | vks = model.vks |> Logic.update { prevState | definition = Dict.remove answerid prevState.definition } }, Cmd.none )


init : List ExperimentInfo.Task -> List Trial -> Logic.Task Trial State
init infos trials =
    let
        info =
            ExperimentInfo.toDict infos |> Dict.get taskId |> Result.fromMaybe "I couldn't find Task infos"
    in
    Logic.startIntro info [] trials initState


initState : State
initState =
    { knowledge = NoAnswer, definition = Dict.empty |> Dict.insert 0 "", usage = "" }


familiarityToString : Familiarity -> String
familiarityToString fam =
    case fam of
        Known ->
            "Known"

        NeverSeen ->
            "NeverSeen"

        PreviouslySeen ->
            "PreviouslySeen"

        _ ->
            ""


familiarityFromString : String -> Familiarity
familiarityFromString str =
    case str of
        "Known" ->
            Known

        "NeverSeen" ->
            NeverSeen

        "PreviouslySeen" ->
            PreviouslySeen

        _ ->
            NoAnswer


saveData responseHandler maybeUserId task =
    let
        history =
            Logic.getHistory task

        userId =
            maybeUserId |> Maybe.withDefault "recd18l2IBRQNI05y"

        summarizedTrialEncoder =
            Encode.list
                (\( { id }, { knowledge, definition, usage } ) ->
                    let
                        lsToString =
                            definition |> Dict.toList |> List.map Tuple.second |> List.intersperse "," |> String.concat
                    in
                    Encode.object
                        [ ( "fields"
                          , Encode.object
                                [ ( "id", Encode.list Encode.string [ id ] )
                                , ( "knowledge", Encode.string (familiarityToString knowledge) )
                                , ( "definition", Encode.string lsToString )
                                , ( "usage", Encode.string usage )
                                ]
                          )
                        ]
                )

        sendInBatch_ =
            Data.sendInBatch summarizedTrialEncoder taskId userId history
    in
    Task.attempt responseHandler sendInBatch_
