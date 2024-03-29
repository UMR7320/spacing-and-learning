module Pretest.YesNo exposing (..)

import Activity exposing (Activity)
import ActivityInfo exposing (ActivityInfo, Session(..))
import Browser.Navigation exposing (Key, pushUrl)
import Data
import Html.Styled exposing (Html, a, button, div, p, text, video)
import Html.Styled.Attributes exposing (class, controls, href, src)
import Html.Styled.Events exposing (onClick)
import Http
import Json.Decode as Decode exposing (Decoder)
import Json.Decode.Pipeline exposing (..)
import Json.Encode as Encode
import RemoteData exposing (RemoteData)
import Route exposing (PretestRoute(..), YesNoRoute(..))
import Task
import Time
import View



-- MODEL


type alias State =
    { evaluation : Maybe Bool }


type alias Trial =
    { id : String
    , word : String
    , exists : Bool
    , isTraining : Bool
    }


type alias YesNo =
    Activity Trial State


type alias Model a =
    { a
        | yesNo : YesNo
        , user : Maybe String
        , activitiesInfos : RemoteData Http.Error (List ActivityInfo)
        , key : Key
    }


initState : State
initState =
    { evaluation = Nothing }



-- UPDATE


type Msg
    = NoOp
    | GotTrials (RemoteData Http.Error (List Trial))
    | UserClickedStartTraining
    | UserClickedStartMain
    | UserPressedButton Bool
    | NextTrial Bool Time.Posix
    | UserClickedSaveData
    | ServerRespondedWithUpdatedUser (Result Http.Error String)
    | HistoryWasSaved (Result Http.Error String)


update : Msg -> Model a -> ( Model a, Cmd Msg )
update msg model =
    case msg of
        GotTrials (RemoteData.Success trials) ->
            ( { model | yesNo = Activity.trialsLoaded trials initState model.yesNo }
            , Cmd.none
            )

        GotTrials (RemoteData.Failure (Http.BadBody error)) ->
            ( { model | yesNo = Activity.Err error }
            , Cmd.none
            )

        GotTrials (RemoteData.Failure _) ->
            ( { model | yesNo = Activity.Err "Failed to fetch trials" }
            , Cmd.none
            )

        GotTrials _ ->
            ( model, Cmd.none )

        UserClickedStartTraining ->
            ( { model | yesNo = Activity.startTraining model.yesNo }
            , pushUrl model.key "instructions"
            )

        UserClickedStartMain ->
            ( { model | yesNo = Activity.startMain model.yesNo initState }
            , pushUrl model.key "../yes-no"
            )

        UserPressedButton bool ->
            ( model, Task.perform (NextTrial bool) Time.now )

        NextTrial bool timestamp ->
            ( { model
                | yesNo =
                    Activity.update { evaluation = Just bool } model.yesNo
                        |> Activity.next timestamp initState
              }
            , if Activity.isRunningMain model.yesNo then
                saveHistory model

              else
                Cmd.none
            )

        UserClickedSaveData ->
            let
                userId =
                    model.user |> Maybe.withDefault "recd18l2IBRQNI05y"

                totalScore =
                    computeLexTaleScore (Activity.getHistory model.yesNo)

                encode =
                    Encode.list
                        (\score ->
                            Encode.object
                                [ ( "id", Encode.string userId )
                                , ( "fields"
                                  , Encode.object
                                        [ ( "YesNo", Encode.float score ) ]
                                  )
                                ]
                        )
                        [ totalScore ]

                responseDecoder =
                    Decode.field "id" Decode.string
            in
            ( { model | yesNo = Activity.Loading Nothing Nothing }
            , updateVocabularyScore (Http.jsonBody encode) ServerRespondedWithUpdatedUser responseDecoder
            )

        ServerRespondedWithUpdatedUser (Result.Err reason) ->
            ( { model | yesNo = Activity.Err (Data.buildErrorMessage reason) }, Cmd.none )

        ServerRespondedWithUpdatedUser _ ->
            ( { model | yesNo = Activity.NotStarted }, Cmd.none )

        -- Ignore errors because we don't want to interrupt the activity
        HistoryWasSaved _ ->
            ( model, Cmd.none )

        NoOp ->
            ( model, Cmd.none )


{-| <https://www.lextale.com/scoring.html>
-}
computeLexTaleScore : List ( Trial, State, Time.Posix ) -> Float
computeLexTaleScore history =
    let
        wordsCorrectCount =
            history
                |> List.filter (\( word, answer, _ ) -> word.exists && answer.evaluation == Just True)
                |> List.length
                |> toFloat

        nonWordCorrectCount =
            history
                |> List.filter (\( word, answer, _ ) -> not word.exists && answer.evaluation == Just False)
                |> List.length
                |> toFloat
    in
    (wordsCorrectCount * 40 / 100 + nonWordCorrectCount * 20 / 100) / 2


correctionFactor : Int -> Int -> Float
correctionFactor falseAlarms hits =
    1 - (weighted falseAlarms / weighted hits)


vocabularyScore : Int -> Int -> Int
vocabularyScore hits falseAlarms =
    (toFloat hits * 100.0 * correctionFactor falseAlarms hits) |> round


weighted : Int -> Float
weighted x =
    case x of
        0 ->
            0

        1 ->
            1

        2 ->
            3

        4 ->
            10

        5 ->
            15

        6 ->
            20

        7 ->
            24

        8 ->
            27

        9 ->
            29

        10 ->
            30

        _ ->
            30


countHitsAndFalseAlarms : List ( Trial, State, c ) -> ( Int, Int )
countHitsAndFalseAlarms =
    List.foldl
        (\( word, answer, _ ) ( hits, falseAlarms ) ->
            if answer.evaluation == Just True then
                if word.exists == True then
                    ( hits + 1, falseAlarms )

                else
                    ( hits, falseAlarms + 1 )

            else
                ( hits, falseAlarms )
        )
        ( 0, 0 )


updateVocabularyScore : Http.Body -> (Result Http.Error a -> msg) -> Decoder a -> Cmd msg
updateVocabularyScore payload callbackMsg decoder =
    Http.request
        { method = "PATCH"
        , headers = []
        , url =
            Data.buildQuery
                { app = Data.apps.spacing
                , base = "users"
                , view_ = "allUsers"
                }
        , body = payload
        , expect = Http.expectJson callbackMsg decoder
        , timeout = Nothing
        , tracker = Nothing
        }



-- VIEW


view : YesNo -> YesNoRoute -> List (Html Msg)
view activity page =
    case page of
        YesNoActivity ->
            viewActivity activity

        YesNoVideo ->
            [ div
                [ class "flow" ]
                [ p [] [ text "Regarde cette vidéo explicative :" ]
                , video
                    [ controls True
                    , src "/yesNo.mp4"
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

        YesNoTrainingInstructions ->
            [ div [ class "endInfo" ]
                [ div []
                    [ div [ class "pb-8" ] [ text "Commençons par 4 exemples" ]
                    , a
                        [ href "../yes-no"
                        , class "button"
                        ]
                        [ text "Continue" ]
                    ]
                ]
            ]


viewActivity : YesNo -> List (Html Msg)
viewActivity activity =
    case activity of
        Activity.NotStarted ->
            [ text "C'est Bon!" ]

        Activity.Loading _ _ ->
            [ View.loading ]

        Activity.Running step data ->
            case step of
                Activity.Instructions ->
                    [ View.unsafeInstructionsWithLink data.infos "yes-no/video" ]

                Activity.Training ->
                    data.current
                        |> Maybe.map viewTrial
                        |> Maybe.withDefault [ View.introToMain UserClickedStartMain ]

                Activity.Main ->
                    data.current
                        |> Maybe.map viewTrial
                        |> Maybe.withDefault [ View.end data.infos.end UserClickedSaveData (Just "vks") ]

        Activity.Err reason ->
            [ p [] [ text reason ] ]


viewTrial : Trial -> List (Html Msg)
viewTrial trial =
    [ div [ class " yes-no" ]
        [ div [ class "text-3xl font-bold italic mb-8 text-center" ] [ text trial.word ]
        , div [ class "yes-no-buttons" ]
            [ button
                [ onClick (UserPressedButton False) ]
                [ text "No" ]
            , button
                [ onClick (UserPressedButton True) ]
                [ text "Yes" ]
            ]
        ]
    ]


infoLoaded : List ActivityInfo -> YesNo -> YesNo
infoLoaded infos =
    Activity.infoLoaded
        Pretest
        "YesNo"
        infos
        initState



-- HTTP


getRecords : Cmd Msg
getRecords =
    Http.get
        { url =
            Data.buildQuery
                { app = Data.apps.spacing
                , base = "yes_no"
                , view_ = "all"
                }
        , expect = Http.expectJson (RemoteData.fromResult >> GotTrials) decodeYesNoTrials
        }


decodeYesNoTrials : Decode.Decoder (List Trial)
decodeYesNoTrials =
    let
        decoder =
            Decode.succeed Trial
                |> required "id" Decode.string
                |> required "ItemName" Decode.string
                |> optional "Exists" Decode.bool False
                |> optional "isTraining" Decode.bool False
    in
    Data.decodeRecords decoder


saveHistory : Model a -> Cmd Msg
saveHistory model =
    let
        history =
            Activity.getHistory model.yesNo

        userId =
            model.user |> Maybe.withDefault "recd18l2IBRQNI05y"

        payload =
            updateHistoryEncoder userId history
    in
    Http.request
        { method = "PATCH"
        , headers = []
        , url = Data.buildQuery { app = Data.apps.spacing, base = "users", view_ = "YesNo_output" }
        , body = Http.jsonBody payload
        , expect = Http.expectJson HistoryWasSaved (Decode.succeed "OK")
        , timeout = Nothing
        , tracker = Nothing
        }


updateHistoryEncoder : String -> List ( Trial, State, Time.Posix ) -> Encode.Value
updateHistoryEncoder userId history =
    -- The Netflify function that receives PATCH requests only works with arrays
    Encode.list
        (\_ ->
            Encode.object
                [ ( "id", Encode.string userId )
                , ( "fields", historyEncoder history )
                ]
        )
        [ ( userId, history ) ]


historyEncoder : List ( Trial, State, Time.Posix ) -> Encode.Value
historyEncoder history =
    Encode.object
        -- airtable does not support JSON columns, so we save giant JSON strings
        [ ( "YesNo_answers", Encode.string (Encode.encode 0 (Encode.list historyItemEncoder history)) )
        ]


historyItemEncoder : ( Trial, State, Time.Posix ) -> Encode.Value
historyItemEncoder ( { id, exists }, { evaluation }, timestamp ) =
    Encode.object
        [ ( "trialId", Encode.string id )
        , ( "wordExists", Encode.bool exists )
        , ( "evaluation", Maybe.map Encode.bool evaluation |> Maybe.withDefault Encode.null )
        , ( "answeredAt", Encode.int (Time.posixToMillis timestamp) )
        ]
