module Pretest.YesNo exposing (..)

import Browser.Events exposing (onKeyDown)
import Data
import Dict
import ExperimentInfo
import Html.Styled exposing (Html, div, p, text)
import Html.Styled.Attributes exposing (class)
import Http
import Json.Decode as Decode exposing (Decoder)
import Json.Decode.Pipeline exposing (..)
import Json.Encode as Encode
import Logic
import Progressbar exposing (progressBar)
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
            ( { model | yesno = Logic.update { evaluation = maybeBool } model.yesno |> Logic.next initState }, Cmd.none )

        UserClickedSaveData ->
            let
                userId =
                    model.user |> Maybe.withDefault "recd18l2IBRQNI05y"

                history =
                    Logic.getHistory model.yesno

                encodeUpdatedUser =
                    Encode.object
                        [ ( "records"
                          , Encode.list
                                (\score ->
                                    Encode.object
                                        [ ( "id", Encode.string userId )
                                        , ( "fields"
                                          , Encode.object
                                                [ ( "VocabLevel", Encode.int score )
                                                ]
                                          )
                                        ]
                                )
                                [ 1000 ]
                          )
                        ]

                encode =
                    Encode.list
                        (\score ->
                            Encode.object
                                [ ( "id", Encode.string userId )
                                , ( "fields"
                                  , Encode.object
                                        [ ( "VocabLevel", Encode.int score )
                                        ]
                                  )
                                ]
                        )
                        [ round scoreVoc ]

                correctionFactor =
                    1 - (weighted falseAlarms / weighted hits)

                scoreVoc =
                    hits * 100 * correctionFactor

                weighted =
                    (*) 0.2

                ( hits, falseAlarms ) =
                    List.foldl
                        (\( t, s ) ( h, f ) ->
                            if t.exists == (s.evaluation |> Maybe.withDefault False) then
                                ( h + 1, f )

                            else
                                ( h, f + 1 )
                        )
                        ( 0, 0 )
                        history

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


type Msg
    = NoOp
    | UserClickedStartTask
    | UserPressedButton (Maybe Bool)
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
            [ text "C'est Bon !" ]

        Logic.Loading ->
            [ View.loading ]

        Logic.Running step data ->
            case step of
                Logic.Instructions ->
                    [ View.instructions data.infos.instructions UserClickedStartTask ]

                Logic.Training ->
                    []

                Logic.Main ->
                    case data.current of
                        Just trial ->
                            [ div [ class "flex flex-col items-center text-xl font-bold" ]
                                [ Progressbar.progressBarWhenNoTraining data.history data.mainTrials
                                , text trial.word
                                , div [ class "flex flex-row m-2" ]
                                    [ unclickableButton "bg-green-500" "F = Exists"
                                    , unclickableButton "bg-red-500" "J = Does not exist"
                                    ]
                                ]
                            ]

                        Nothing ->
                            [ View.end data.infos.end UserClickedSaveData "" ]

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
