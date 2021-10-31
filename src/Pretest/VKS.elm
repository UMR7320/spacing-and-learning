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
import Url.Builder
import View



-- MODEL


type alias Model superModel =
    { superModel
        | vks : Logic.Task Trial Answer
        , vksOutputId : Maybe String
        , user : Maybe String
        , version : Maybe String
    }


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


type alias Answer =
    { knowledge : Familiarity
    , definition : String
    , translation : String
    , synonym : String
    , usage : String
    }


type alias SC =
    Logic.Task Trial Answer


toTask : List ExperimentInfo.Task -> List Trial -> Model superModel -> Logic.Task Trial Answer
toTask infos trials model =
    let
        info =
            ExperimentInfo.toDict infos
                |> Dict.get (taskId model)
                |> Result.fromMaybe "I couldn't find Task infos"
    in
    Logic.startIntro info [] trials emptyAnswer


emptyAnswer : Answer
emptyAnswer =
    { knowledge = NoAnswer
    , definition = ""
    , synonym = ""
    , translation = ""
    , usage = ""
    }



-- VIEW


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



-- UPDATE


type Msg
    = UserClickedNextTrial
    | UserClickedStartMain
    | UserClickedSaveData
    | UserUpdatedField Field Int String
    | RuntimeReordedAmorces Field
    | UserClickedNewKnowledge String
    | GotOutputId (Result Http.Error (List String))
    | OutputRecordWasCreated (Result Http.Error String)
    | OutputRecordWasUpdated (Result Http.Error String)


type Field
    = Definition
    | Synonym
    | Translation
    | SecondProduction


update : Msg -> Model superModel -> ( Model superModel, Cmd Msg )
update msg model =
    let
        prevAnswer =
            Logic.getState model.vks |> Maybe.withDefault emptyAnswer
    in
    case msg of
        GotOutputId (Ok id) ->
            ( { model | vksOutputId = List.head id }, Cmd.none )

        GotOutputId (Err _) ->
            ( model, Cmd.none )

        OutputRecordWasCreated (Ok id) ->
            ( { model | vksOutputId = Just id }, Cmd.none )

        OutputRecordWasCreated (Err _) ->
            ( model, Cmd.none )

        OutputRecordWasUpdated _ ->
            ( model, Cmd.none )

        RuntimeReordedAmorces _ ->
            ( { model | vks = model.vks |> Logic.update prevAnswer }, Cmd.none )

        UserClickedNextTrial ->
            let
                newModel =
                    { model | vks = model.vks |> Logic.toggle |> Logic.next emptyAnswer }
            in
            ( newModel
            , Cmd.batch
                [ Random.generate RuntimeReordedAmorces (Random.uniform Definition [ SecondProduction ])
                , saveData newModel
                ]
            )

        UserClickedStartMain ->
            ( { model | vks = Logic.startMain model.vks emptyAnswer }, Cmd.none )

        UserUpdatedField fieldId subKey new ->
            case fieldId of
                Definition ->
                    ( { model | vks = model.vks |> Logic.update { prevAnswer | definition = new } }, Cmd.none )

                Synonym ->
                    ( { model | vks = model.vks |> Logic.update { prevAnswer | synonym = new } }, Cmd.none )

                Translation ->
                    ( { model | vks = model.vks |> Logic.update { prevAnswer | translation = new } }, Cmd.none )

                SecondProduction ->
                    ( { model | vks = model.vks |> Logic.update { prevAnswer | usage = new } }, Cmd.none )

        UserClickedNewKnowledge str ->
            ( { model | vks = model.vks |> Logic.update { prevAnswer | knowledge = familiarityFromString str } }, Cmd.none )

        UserClickedSaveData ->
            ( { model | vks = Logic.Loading }, Cmd.none )



-- HTTP


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


{-| Fetches the id of the record that holds the user's answers
-}
fetchOutputId : String -> Cmd Msg
fetchOutputId userId =
    Http.get
        { url =
            Url.Builder.absolute
                [ ".netlify"
                , "functions"
                , "api"
                ]
                [ Url.Builder.string "app" Data.apps.spacing
                , Url.Builder.string "base" "VKS_output"
                , Url.Builder.string "view" "all"
                , Url.Builder.string "filterByFormula" ("{LinkToUserUID} = '" ++ userId ++ "'")
                ]
        , expect = Http.expectJson GotOutputId outputIdDecoder
        }


outputIdDecoder =
    Decode.field "records" (Decode.list (Decode.field "id" Decode.string))


historyEncoder : String -> String -> List ( Trial, Answer ) -> Encode.Value
historyEncoder version userId history =
    let
        answerField =
            case version of
                "post" ->
                    "PostTestAnswers"

                "post-diff" ->
                    "PostTestDiffAnswers"

                "surprise" ->
                    "SurprisePostTestAnswers"

                _ ->
                    "PreTestAnswers"
    in
    Encode.object
        -- The Airtable API forces link to other records to be arrays
        [ ( "LinkToUserUID", Encode.list Encode.string [ userId ] )

        -- airtable does not support JSON columns, so Answers is a giant JSON string
        , ( answerField, Encode.string (Encode.encode 0 (Encode.list historyItemEncoder history)) )
        ]


historyItemEncoder : ( Trial, Answer ) -> Encode.Value
historyItemEncoder ( { id }, { knowledge, definition, translation, synonym, usage } ) =
    Encode.object
        [ ( "trialUID", Encode.string id )
        , ( "vks_knowledge", Encode.string (familiarityToString knowledge) )
        , ( "vks_definition", Encode.string definition )
        , ( "vks_synonym", Encode.string synonym )
        , ( "vks_translation", Encode.string translation )
        , ( "vks_usage", Encode.string usage )
        ]


updateHistoryEncoder : String -> String -> String -> List ( Trial, Answer ) -> Encode.Value
updateHistoryEncoder outputId version userId history =
    -- The Airtable API forces PATCH requests to send arrays
    Encode.list
        (\_ ->
            Encode.object
                [ ( "id", Encode.string outputId )
                , ( "fields", historyEncoder version userId history )
                ]
        )
        [ ( version, userId, history ) ]


saveData model =
    let
        history =
            Logic.getHistory model.vks

        userId =
            model.user |> Maybe.withDefault "recd18l2IBRQNI05y"

        version =
            Maybe.withDefault "pre" model.version
    in
    case model.vksOutputId of
        Nothing ->
            createOutputRecord version userId history

        Just id ->
            updateOutputRecord id version userId history


createOutputRecord version userId history =
    let
        payload =
            historyEncoder version userId history
    in
    Http.post
        { url =
            Data.buildQuery
                { app = Data.apps.spacing
                , base = "VKS_output"
                , view_ = "all"
                }
        , body = Http.jsonBody payload
        , expect = Http.expectJson OutputRecordWasCreated (Decode.field "id" Decode.string)
        }


updateOutputRecord outputId version userId history =
    let
        payload =
            updateHistoryEncoder outputId version userId history
    in
    Http.request
        { method = "PATCH"
        , headers = []
        , url =
            Data.buildQuery
                { app = Data.apps.spacing
                , base = "VKS_output"
                , view_ = "all"
                }
        , body = Http.jsonBody payload
        , expect = Http.expectJson OutputRecordWasUpdated (Decode.field "id" Decode.string)
        , timeout = Nothing
        , tracker = Nothing
        }



-- INTERNAL


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
