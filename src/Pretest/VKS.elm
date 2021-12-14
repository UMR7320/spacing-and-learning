module Pretest.VKS exposing (..)

import Data
import Dict
import ExperimentInfo
import Html.Attributes exposing (placeholder)
import Html.Styled exposing (..)
import Html.Styled.Attributes as A exposing (class, controls, src, type_)
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
        | vks : { task : Logic.Task Trial Answer, showVideo : Bool }
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
    , usage = ""
    }



-- VIEW


view : { task : SC, showVideo : Bool } -> List (Html Msg)
view vks =
    case vks.task of
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
                                    [ text "What does this verb mean? Give definitions, synonyms, and/or translations, as many as you can."
                                    , input [ class "border-2", E.onInput (UserUpdatedField Definition) ] []
                                    ]
                                , label [ class "flex flex-col p-2" ]
                                    [ text "Please use this verb in a sentence. The sentence should show that you know what the word means."
                                    , textarea [ class "border-2", E.onInput (UserUpdatedField Sentence) ] []
                                    ]
                                ]

                          else
                            text ""
                        , View.button
                            { txt = "Next Item"
                            , message = UserClickedNextTrial
                            , isDisabled =
                                if data.state.knowledge == Known && List.all String.isEmpty [ data.state.definition ] then
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
            if vks.showVideo then
                [ div
                    [ class "flow" ]
                    [ p [] [ text "Watch the video to understand how this activity works:" ]
                    , video [ controls True, src "/vks.mp4" ] []
                    , View.button
                        { message = UserClickedStartMain
                        , txt = "Start the activity"
                        , isDisabled = False
                        }
                    ]
                ]

            else
                [ View.instructions data.infos UserClickedShowVideo ]



-- UPDATE


type Msg
    = UserClickedNextTrial
    | UserClickedStartMain
    | UserClickedShowVideo
    | UserClickedSaveData
    | UserUpdatedField Field String
    | RuntimeReordedAmorces Field
    | UserClickedNewKnowledge String
    | HistoryWasSaved (Result Http.Error String)


type Field
    = Definition
    | Sentence


update : Msg -> Model superModel -> ( Model superModel, Cmd Msg )
update msg model =
    let
        prevAnswer =
            Logic.getState model.vks.task |> Maybe.withDefault emptyAnswer

        vks =
            model.vks

        updateTask f =
            { vks | task = f vks.task }
    in
    case msg of
        HistoryWasSaved _ ->
            ( model, Cmd.none )

        RuntimeReordedAmorces _ ->
            ( { model | vks = updateTask (Logic.update prevAnswer) }, Cmd.none )

        UserClickedNextTrial ->
            let
                newModel =
                    { model | vks = updateTask (Logic.toggle >> Logic.next emptyAnswer) }
            in
            ( newModel
            , Cmd.batch
                [ Random.generate RuntimeReordedAmorces (Random.uniform Definition [ Sentence ])
                , saveData newModel
                ]
            )

        UserClickedShowVideo ->
            let
                updatedVks =
                    { vks | showVideo = True }
            in
            ( { model | vks = updatedVks }, Cmd.none )

        UserClickedStartMain ->
            let
                -- gross hack
                flippedStartMain a b =
                    Logic.startMain b a
            in
            ( { model | vks = updateTask (flippedStartMain emptyAnswer) }, Cmd.none )

        UserUpdatedField fieldId new ->
            case fieldId of
                Definition ->
                    ( { model
                        | vks = updateTask (Logic.update { prevAnswer | definition = new })
                      }
                    , Cmd.none
                    )

                Sentence ->
                    ( { model
                        | vks = updateTask (Logic.update { prevAnswer | usage = new })
                      }
                    , Cmd.none
                    )

        UserClickedNewKnowledge str ->
            ( { model
                | vks = updateTask (Logic.update { prevAnswer | knowledge = familiarityFromString str })
              }
            , Cmd.none
            )

        UserClickedSaveData ->
            ( { model | vks = updateTask (always Logic.Loading) }, Cmd.none )



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


historyEncoder : String -> String -> List ( Trial, Answer ) -> Encode.Value
historyEncoder version userId history =
    let
        answerField =
            case version of
                "post" ->
                    "VKS_postTest"

                "post-diff" ->
                    "VKS_postTestDiff"

                "surprise" ->
                    "VKS_surprisePostTest"

                _ ->
                    "VKS_preTest"
    in
    Encode.object
        -- airtable does not support JSON columns, so we save giant JSON strings
        [ ( answerField, Encode.string (Encode.encode 0 (Encode.list historyItemEncoder history)) )
        ]


historyItemEncoder : ( Trial, Answer ) -> Encode.Value
historyItemEncoder ( { id, verb }, { knowledge, definition, usage } ) =
    Encode.object
        [ ( "verb", Encode.string verb )
        , ( "vks_knowledge", Encode.string (familiarityToString knowledge) )
        , ( "vks_definition", Encode.string definition )
        , ( "vks_usage", Encode.string usage )
        ]


updateHistoryEncoder : String -> String -> List ( Trial, Answer ) -> Encode.Value
updateHistoryEncoder version userId history =
    -- The Netflify function that receives PATCH requests only works with arrays
    Encode.list
        (\_ ->
            Encode.object
                [ ( "id", Encode.string userId )
                , ( "fields", historyEncoder version userId history )
                ]
        )
        [ ( version, userId, history ) ]


saveData model =
    let
        history =
            Logic.getHistory model.vks.task

        userId =
            model.user |> Maybe.withDefault "recd18l2IBRQNI05y"

        version =
            Maybe.withDefault "pre" model.version

        payload =
            updateHistoryEncoder version userId history
    in
    Http.request
        { method = "PATCH"
        , headers = []
        , url = Data.buildQuery { app = Data.apps.spacing, base = "users", view_ = "VKS_output" }
        , body = Http.jsonBody payload
        , expect = Http.expectJson HistoryWasSaved (Decode.succeed "OK")
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
