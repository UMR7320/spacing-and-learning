module Pretest.VKS exposing (..)

import Activity exposing (Activity)
import ActivityInfo exposing (ActivityInfo, Session(..))
import Browser.Navigation exposing (Key, pushUrl)
import Data
import Html.Styled exposing (..)
import Html.Styled.Attributes as A exposing (class, controls, href, src, type_)
import Html.Styled.Events as E
import Http
import Json.Decode as Decode
import Json.Decode.Pipeline exposing (optional, required)
import Json.Encode as Encode
import Pretest.Version exposing (Version(..))
import Random
import Random.List exposing (shuffle)
import RemoteData exposing (RemoteData)
import Route exposing (VKSRoute(..))
import Task
import Time
import View



-- MODEL


type alias Model superModel =
    { superModel
        | vks : VKS
        , user : Maybe String
        , version : Version
        , key : Key
    }


type alias Trial =
    { id : String
    , verb : String
    , isTraining : Bool
    , isExperimental : Bool
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
    , error : Maybe String
    }


type alias VKS =
    Activity Trial Answer


toActivity : List ActivityInfo -> List Trial -> Version -> Activity Trial Answer
toActivity infos trials version =
    Activity.startIntro
        (ActivityInfo.activityInfo infos (Pretest.Version.toSession version) "LexLearn verbs")
        []
        trials
        emptyAnswer


emptyAnswer : Answer
emptyAnswer =
    { knowledge = NoAnswer
    , definition = ""
    , usage = ""
    , error = Nothing
    }


infoLoaded : List ActivityInfo -> VKS -> VKS
infoLoaded infos =
    Activity.infoLoaded
        Pretest
        "VKS"
        infos
        emptyAnswer



-- VIEW


view : VKS -> VKSRoute -> List (Html Msg)
view vks page =
    case page of
        VKSActivity ->
            viewActivity vks

        VKSVideo ->
            [ div
                [ class "flow" ]
                [ p [] [ text "Regarde cette vidéo explicative :" ]
                , video
                    [ controls True
                    , src "/vks.mp4"
                    , class "border-2"
                    ]
                    []
                , View.button
                    { message = UserClickedStartMain
                    , txt = "Je commence mon quizz"
                    , isDisabled = False
                    }
                ]
            ]

        VKSTrainingInstructions ->
            [ div [ class "endInfo" ]
                [ div []
                    [ div [ class "pb-8" ] [ text "Commençons par quelques exemples" ]
                    , a
                        [ href "../vks"
                        , class "button"
                        ]
                        [ text "Continue" ]
                    ]
                ]
            ]


viewActivity : VKS -> List (Html Msg)
viewActivity activity =
    case activity of
        Activity.Running Activity.Training data ->
            data.current
                |> Maybe.map (viewTrial data)
                |> Maybe.withDefault [ View.introToMain UserClickedStartMain ]

        Activity.Running Activity.Main data ->
            data.current
                |> Maybe.map (viewTrial data)
                |> Maybe.withDefault [ View.end data.infos.end UserClickedSaveData Nothing ]

        Activity.Err reason ->
            [ text reason ]

        Activity.Loading _ _ ->
            [ View.loading ]

        Activity.NotStarted ->
            [ text "C'est tout bon!" ]

        Activity.Running Activity.Instructions data ->
            [ View.unsafeInstructionsWithLink data.infos "vks/video" ]


viewTrial : Activity.Data Trial Answer -> Trial -> List (Html Msg)
viewTrial data trial =
    [ div [ A.class "vks flex flex-col items-center flow" ]
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
                , span [ class "p-2" ] [ text "Je n'ai jamais vu ce verbe" ]
                ]
            , Html.Styled.label []
                [ Html.Styled.input
                    [ type_ "radio"
                    , A.value "PreviouslySeen"
                    , A.checked (data.state.knowledge == PreviouslySeen)
                    , E.onInput UserClickedNewKnowledge
                    ]
                    []
                , span [ class "p-2" ] [ text "J'ai déjà vu ce verbe mais je ne sais pas le traduire" ]
                ]
            , Html.Styled.label []
                [ Html.Styled.input
                    [ type_ "radio"
                    , A.value "Known"
                    , A.checked (data.state.knowledge == Known)
                    , E.onInput UserClickedNewKnowledge
                    ]
                    []
                , span [ class "p-2" ] [ text "J'ai déjà vu ce verbe et je sais le traduire" ]
                ]
            ]
        , if data.state.knowledge == Known then
            Html.Styled.fieldset [ class "flex flex-col p-2" ]
                [ label
                    [ class "flex flex-col" ]
                    [ text "Voici ma traduction :"
                    , input
                        [ class "border-2 p-2 mt-2"
                        , E.onInput (UserUpdatedField Definition)
                        ]
                        []
                    ]
                , case data.state.error of
                    Just error ->
                        div
                            [ class "text-red-600" ]
                            [ text error ]

                    Nothing ->
                        text ""
                , label [ class "flex flex-col mt-4" ]
                    [ text "Je sais aussi utiliser ce verbe dans une phrase. Voici ma phrase :"
                    , textarea
                        [ class "border-2 p-2 mt-2"
                        , E.onInput (UserUpdatedField Sentence)
                        ]
                        []
                    ]
                ]

          else
            text ""
        , View.button
            { txt = "Continue"
            , message = UserClickedNextTrial
            , isDisabled = data.state.knowledge == NoAnswer
            }
        ]
    ]



-- UPDATE


type Msg
    = UserClickedNextTrial
    | GotTrials (RemoteData Http.Error (List Trial))
    | GotRandomizedTrials (List Trial)
    | NextTrial Time.Posix
    | UserClickedStartTraining
    | UserClickedStartMain
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
            Activity.getState model.vks |> Maybe.withDefault emptyAnswer
    in
    case msg of
        GotTrials (RemoteData.Success trials) ->
            ( model
            , Random.generate GotRandomizedTrials (shuffle (List.filter .isExperimental trials))
            )

        GotRandomizedTrials trials ->
            ( { model | vks = Activity.trialsLoaded trials emptyAnswer model.vks }
            , Cmd.none
            )

        GotTrials (RemoteData.Failure error) ->
            ( { model
                | vks = Activity.Err (Data.buildErrorMessage error)
              }
            , Cmd.none
            )

        GotTrials _ ->
            ( model, Cmd.none )

        HistoryWasSaved _ ->
            ( model, Cmd.none )

        RuntimeReordedAmorces _ ->
            ( { model | vks = Activity.update prevAnswer model.vks }, Cmd.none )

        UserClickedNextTrial ->
            if prevAnswer.knowledge == Known && String.isEmpty prevAnswer.definition then
                ( { model
                    | vks =
                        Activity.update
                            { prevAnswer | error = Just "Ce champ est obligatoire" }
                            model.vks
                  }
                , Cmd.none
                )

            else
                ( model, Task.perform NextTrial Time.now )

        NextTrial timestamp ->
            let
                newModel =
                    { model | vks = (Activity.toggle >> Activity.next timestamp emptyAnswer) model.vks }
            in
            ( newModel
            , Cmd.batch
                [ Random.generate RuntimeReordedAmorces (Random.uniform Definition [ Sentence ])
                , if Activity.isRunningMain newModel.vks then
                    saveData newModel

                  else
                    Cmd.none
                ]
            )

        UserClickedStartTraining ->
            ( { model | vks = Activity.startTraining model.vks }
            , pushUrl model.key "instructions"
            )

        UserClickedStartMain ->
            ( { model | vks = Activity.startMain model.vks emptyAnswer }
            , pushUrl model.key "../vks"
            )

        UserUpdatedField fieldId new ->
            case fieldId of
                Definition ->
                    ( { model
                        | vks =
                            Activity.update
                                { prevAnswer | definition = new, error = Nothing }
                                model.vks
                      }
                    , Cmd.none
                    )

                Sentence ->
                    ( { model
                        | vks = Activity.update { prevAnswer | usage = new } model.vks
                      }
                    , Cmd.none
                    )

        UserClickedNewKnowledge str ->
            ( { model
                | vks =
                    Activity.update
                        { prevAnswer
                            | knowledge = familiarityFromString str
                            , error = Nothing
                        }
                        model.vks
              }
            , Cmd.none
            )

        UserClickedSaveData ->
            ( { model | vks = Activity.Loading Nothing Nothing }, Cmd.none )



-- HTTP


getRecords : Cmd Msg
getRecords =
    Http.get
        { url =
            Data.buildQuery
                { app = Data.apps.spacing
                , base = "input"
                , view_ = "Meaning"
                }
        , expect = Http.expectJson (RemoteData.fromResult >> GotTrials) decodeVKSTrials
        }


decodeVKSTrials : Decode.Decoder (List Trial)
decodeVKSTrials =
    let
        decoder =
            Decode.succeed Trial
                |> required "id" Decode.string
                |> required "Word_Text" Decode.string
                |> optional "isTraining" Decode.bool False
                |> optional "isExperimental" Decode.bool False
    in
    Data.decodeRecords decoder


historyEncoder : Version -> List ( Trial, Answer, Time.Posix ) -> Encode.Value
historyEncoder version history =
    let
        answerField =
            case version of
                PreTest ->
                    "VKS_preTest"

                PostTest ->
                    "VKS_postTest"

                PostTestDiff ->
                    "VKS_postTestDiff"

                Surprise ->
                    "VKS_surprisePostTest"

                Unknown val ->
                    "VKS_" ++ val
    in
    Encode.object
        -- airtable does not support JSON columns, so we save giant JSON strings
        [ ( answerField, Encode.string (Encode.encode 0 (Encode.list historyItemEncoder history)) )
        ]


historyItemEncoder : ( Trial, Answer, Time.Posix ) -> Encode.Value
historyItemEncoder ( { id, verb }, { knowledge, definition, usage }, timestamp ) =
    Encode.object
        [ ( "trialId", Encode.string id )
        , ( "verb", Encode.string verb )
        , ( "vks_knowledge", Encode.string (familiarityToString knowledge) )
        , ( "vks_definition", Encode.string definition )
        , ( "vks_usage", Encode.string usage )
        , ( "answeredAt", Encode.int (Time.posixToMillis timestamp) )
        ]


updateHistoryEncoder : Version -> String -> List ( Trial, Answer, Time.Posix ) -> Encode.Value
updateHistoryEncoder version userId history =
    -- The Netflify function that receives PATCH requests only works with arrays
    Encode.list
        (\_ ->
            Encode.object
                [ ( "id", Encode.string userId )
                , ( "fields", historyEncoder version history )
                ]
        )
        [ ( version, userId, history ) ]


saveData : Model a -> Cmd Msg
saveData model =
    let
        history =
            Activity.getHistory model.vks

        userId =
            model.user |> Maybe.withDefault "recd18l2IBRQNI05y"

        payload =
            updateHistoryEncoder model.version userId history
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
