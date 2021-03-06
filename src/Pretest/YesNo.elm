module Pretest.YesNo exposing (..)

import Browser.Events exposing (onKeyDown)
import Data
import Dict
import ExperimentInfo
import Html.Styled exposing (Html, div, kbd, p, text)
import Html.Styled.Attributes exposing (class)
import Http
import Json.Decode as Decode exposing (Decoder)
import Json.Decode.Pipeline exposing (..)
import Json.Encode as Encode
import Logic
import Task
import Time
import View exposing (unclickableButton)


getRecords =
    Http.task
        { method = "GET"
        , headers = []
        , url =
            Data.buildQuery
                { app = Data.apps.spacing
                , base = "yes_no"
                , view_ = "all"
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
                |> required "ItemName" Decode.string
                |> optional "Exists" Decode.bool False
    in
    Data.decodeRecords decoder


type alias State =
    { evaluation : Maybe Bool }


initState =
    { evaluation = Nothing }


type alias Trial =
    { id : String, word : String, exists : Bool }


update msg model =
    case msg of
        UserClickedStartTask ->
            ( { model | yesno = Logic.startMain model.yesno initState }, Cmd.none )

        UserPressedButton maybeBool ->
            ( model, Task.perform (NextTrial maybeBool) Time.now )

        NextTrial maybeBool timestamp ->
            ( { model | yesno = Logic.update { evaluation = maybeBool } model.yesno |> Logic.next timestamp initState }, Cmd.none )

        UserClickedSaveData ->
            let
                userId =
                    model.user |> Maybe.withDefault "recd18l2IBRQNI05y"

                totalScore =
                    Logic.getHistory model.yesno
                        |> Data.splitIn 20
                        |> List.map
                            (\block ->
                                let
                                    ( hits, falseAlarms ) =
                                        countHitsAndFalseAlarms block
                                in
                                vocabularyScore hits falseAlarms
                            )
                        |> List.sum

                encode =
                    Encode.list
                        (\score ->
                            Encode.object
                                [ ( "id", Encode.string userId )
                                , ( "fields"
                                  , Encode.object
                                        [ ( "YesNo", Encode.int score ) ]
                                  )
                                ]
                        )
                        [ totalScore ]

                responseDecoder =
                    Decode.field "id" Decode.string
            in
            ( { model | yesno = Logic.Loading }, updateVocabularyScore (Http.jsonBody encode) ServerRespondedWithUpdatedUser responseDecoder )

        ServerRespondedWithUpdatedUser (Result.Err reason) ->
            ( { model | yesno = Logic.Err (Data.buildErrorMessage reason) }, Cmd.none )

        ServerRespondedWithUpdatedUser id ->
            ( { model | yesno = Logic.NotStarted }, Cmd.none )

        _ ->
            ( model, Cmd.none )


correctionFactor : Int -> Int -> Float
correctionFactor falseAlarms hits =
    1 - (weighted falseAlarms / weighted hits)


vocabularyScore : Int -> Int -> Int
vocabularyScore hits falseAlarms =
    (toFloat hits * 100.0 * correctionFactor falseAlarms hits) |> round


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


type Msg
    = NoOp
    | UserClickedStartTask
    | UserPressedButton (Maybe Bool)
    | NextTrial (Maybe Bool) Time.Posix
    | UserClickedSaveData
    | ServerRespondedWithUpdatedUser (Result Http.Error String)


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


type alias YN =
    Logic.Task Trial State


view : YN -> List (Html Msg)
view task =
    case task of
        Logic.NotStarted ->
            [ text "C'est Bon!" ]

        Logic.Loading ->
            [ View.loading ]

        Logic.Running step data ->
            case step of
                Logic.Instructions ->
                    [ View.unsafeInstructions data.infos UserClickedStartTask ]

                Logic.Training ->
                    []

                Logic.Main ->
                    case data.current of
                        Just trial ->
                            [ div []
                                [ div [ class "text-3xl font-bold italic my-6 text-center" ] [ text trial.word ]
                                , div [ class "yes-no-buttons" ]
                                    [ unclickableButton
                                        "bg-gray-300"
                                        [ kbd
                                            []
                                            [ text "f" ]
                                        , text " = I don't know or I'm not sure"
                                        ]
                                    , unclickableButton
                                        "bg-green-500 text-white"
                                        [ kbd
                                            []
                                            [ text "j" ]
                                        , text "I know this word"
                                        ]
                                    ]
                                ]
                            ]

                        Nothing ->
                            [ View.end data.infos.end UserClickedSaveData "vks" ]

        Logic.Err reason ->
            [ p [] [ text reason ] ]


keyDecoder : Decode.Decoder Msg
keyDecoder =
    Decode.map toEvaluation (Decode.field "key" Decode.string)



--toEvaluation : String -> Msg
--toEvaluation : String -> Msg


toEvaluation : String -> Msg
toEvaluation x =
    case x of
        "j" ->
            UserPressedButton (Just True)

        "f" ->
            UserPressedButton (Just False)

        _ ->
            NoOp


subscriptions model =
    case model.yesno of
        Logic.Running Logic.Main _ ->
            Sub.batch [ onKeyDown keyDecoder ]

        _ ->
            Sub.none


taskId =
    "rechYdq4MyLcb2nRG"


init infos trials =
    let
        info =
            ExperimentInfo.toDict infos |> Dict.get taskId |> Result.fromMaybe "I couldn't find Task infos"
    in
    Logic.startIntro info [] trials initState
