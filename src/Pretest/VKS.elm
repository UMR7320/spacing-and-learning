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
import Random
import Task
import View


taskId model =
    case model.version of
        Nothing ->
            "recR6grI83e1so6Zl"

        Just specifiedVersion ->
            case specifiedVersion of
                "pre" ->
                    "recR6grI83e1so6Zl"

                "post" ->
                    "recAlZreFoiVGfSbN"

                "post-diff" ->
                    "rec4WlQXhH06yutJ5"

                "surprise" ->
                    "recYe82wpkoFoW39K"

                _ ->
                    "recR6grI83e1so6Zl"


type alias SC =
    Logic.Task Trial State


view : SC -> List (Html Msg)
view task =
    case task of
        Logic.Running Logic.Training data ->
            case data.current of
                Just _ ->
                    [ text "vks" ]

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
                    [ div [ A.class "flex flex-col items-center flow" ]
                        -- [ View.tooltip data.infos.instructions_short
                        [ div [ class "text-3xl font-bold italic my-6" ] [ text ("to " ++ trial.verb) ]
                        , Html.Styled.fieldset [ class "flex flex-col m-2 flow" ]
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
                                [ label [ class "flex flex-col p-2" ]
                                    [ text "What do you think this verb means? (give as many answers as you can.)"
                                    , input [ class "border-2", E.onInput (UserUpdatedField Definition 1) ] []
                                    ]
                                , label [ class "flex flex-col p-2" ]
                                    [ text "What would be a good translation?"
                                    , input [ class "border-2", E.onInput (UserUpdatedField Translation 1) ] []
                                    ]
                                , label [ class "flex flex-col p-2" ]
                                    [ text "What would be a good synonym?"
                                    , input [ class "border-2", E.onInput (UserUpdatedField Synonym 1) ] []
                                    ]
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
                                if data.state.knowledge == Known && List.all String.isEmpty [ data.state.translation, data.state.definition, data.state.synonym ] then
                                    True

                                else if data.state.knowledge == NoAnswer then
                                    True

                                else
                                    False
                            }
                        ]
                    ]

                Nothing ->
                    [ View.end data.infos.end UserClickedSaveData "spr" ]

        Logic.Err reason ->
            [ text reason ]

        Logic.Loading ->
            [ View.loading ]

        Logic.NotStarted ->
            [ text "C'est tout bon!" ]

        Logic.Running Logic.Instructions data ->
            [ View.instructions data.infos UserClickedStartMain ]


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
    , definition : String
    , translation : String
    , synonym : String
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



--| UserClickedAddAnswer
--| UserClickedRemoveAnswer Int


type Field
    = Definition
    | Synonym
    | Translation
    | SecondProduction


type alias Model superModel =
    { superModel | vks : Logic.Task Trial State, user : Maybe String, version : Maybe String }


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
            ( { model | vks = model.vks |> Logic.toggle |> Logic.next initState }, Random.generate RuntimeReordedAmorces (Random.uniform Definition [ SecondProduction ]) )

        UserClickedStartMain ->
            ( { model | vks = Logic.startMain model.vks initState }, Cmd.none )

        UserUpdatedField fieldId subKey new ->
            case fieldId of
                Definition ->
                    ( { model | vks = model.vks |> Logic.update { prevState | definition = new } }, Cmd.none )

                Synonym ->
                    ( { model | vks = model.vks |> Logic.update { prevState | synonym = new } }, Cmd.none )

                Translation ->
                    ( { model | vks = model.vks |> Logic.update { prevState | translation = new } }, Cmd.none )

                SecondProduction ->
                    ( { model | vks = model.vks |> Logic.update { prevState | usage = new } }, Cmd.none )

        UserClickedSaveData ->
            let
                responseHandler =
                    ServerRespondedWithLastRecords
            in
            ( { model | vks = Logic.Loading }, saveData responseHandler model )

        ServerRespondedWithLastRecords (Result.Ok _) ->
            ( { model | vks = Logic.NotStarted }, Cmd.none )

        ServerRespondedWithLastRecords (Result.Err reason) ->
            ( { model | vks = Logic.Err (Data.buildErrorMessage reason) }, Cmd.none )

        UserClickedNewKnowledge str ->
            ( { model | vks = model.vks |> Logic.update { prevState | knowledge = familiarityFromString str } }, Cmd.none )


init : List ExperimentInfo.Task -> List Trial -> Model superModel -> Logic.Task Trial State
init infos trials model =
    let
        info =
            ExperimentInfo.toDict infos |> Dict.get (taskId model) |> Result.fromMaybe "I couldn't find Task infos"
    in
    Logic.startIntro info [] trials initState


initState : State
initState =
    { knowledge = NoAnswer, definition = "", synonym = "", translation = "", usage = "" }


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


saveData responseHandler model =
    let
        history =
            Logic.getHistory model.vks

        userId =
            model.user |> Maybe.withDefault "recd18l2IBRQNI05y"

        summarizedTrialEncoder =
            Encode.list
                (\( { id }, { knowledge, definition, translation, synonym, usage } ) ->
                    Encode.object
                        [ ( "fields"
                          , Encode.object
                                [ ( "trialUid", Encode.list Encode.string [ id ] )
                                , ( "vks_knowledge", Encode.string (familiarityToString knowledge) )
                                , ( "vks_definition", Encode.string definition )
                                , ( "vks_synonym", Encode.string synonym )
                                , ( "vks_translation", Encode.string translation )
                                , ( "vks_usage", Encode.string usage )
                                , ( "Task_UID", Encode.list Encode.string [ taskId model ] )
                                , ( "userUid", Encode.list Encode.string [ userId ] )
                                ]
                          )
                        ]
                )

        sendInBatch_ =
            Data.sendInBatch summarizedTrialEncoder (taskId model) userId history
    in
    Task.attempt responseHandler sendInBatch_
