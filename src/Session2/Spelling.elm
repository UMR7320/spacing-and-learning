module Session2.Spelling exposing (..)

import Data
import Dict
import DnDList
import ExperimentInfo
import Html.Styled exposing (..)
import Html.Styled.Attributes exposing (class)
import Html.Styled.Events
import Html.Styled.Keyed as Keyed
import Http exposing (Error)
import Icons
import Json.Decode as Decode exposing (Decoder, string)
import Json.Decode.Pipeline exposing (..)
import Logic
import Ports
import Progressbar
import View


getTrialsFromServer : (Result Error (List Trial) -> msg) -> Cmd msg
getTrialsFromServer msgHandler =
    Data.getTrialsFromServer_ "input" "SpellingLvl2" msgHandler decodeTranslationInput


decodeTranslationInput : Decoder (List Trial)
decodeTranslationInput =
    let
        decoder =
            Decode.succeed Trial
                |> required "UID" string
                |> required "Word_Text" string
                |> required "Word_Audio" Data.decodeAudioFiles
                |> optional "isTraining" Decode.bool False
                |> required "Word_Text" string
    in
    Data.decodeRecords decoder


initState : State
initState =
    State "DefaultUid" "" [] 3


defaultTrial : Trial
defaultTrial =
    Trial "defaultTrial" "defaultTrial" (Data.AudioFile "" "") False ""


type alias Trial =
    { uid : String
    , writtenWord : String
    , audioWord : Data.AudioFile
    , isTraining : Bool
    , target : String
    }


type alias State =
    { uid : String
    , userAnswer : String
    , scrambledLetter : List KeyedItem
    , remainingListenings : Int
    }


type alias Item =
    String


type alias KeyedItem =
    ( String, Item )


dedupeHelper : List String -> List ( String, Int ) -> List ( String, Int )
dedupeHelper letters acc =
    let
        lettersInAcc =
            List.map Tuple.first acc

        countRecLetters target =
            List.foldr
                (\letter acc_ ->
                    if target == letter then
                        acc_ + 1

                    else
                        acc_
                )
                1
                lettersInAcc
    in
    case letters of
        [] ->
            acc |> List.reverse

        x :: xs ->
            if List.member x lettersInAcc then
                dedupeHelper xs <|
                    ( x
                    , countRecLetters x
                    )
                        :: acc

            else
                dedupeHelper xs <| ( x, 1 ) :: acc


dedupe : List String -> List ( String, Int )
dedupe letters =
    dedupeHelper letters []


start : List ExperimentInfo.Task -> List Trial -> Logic.Task Trial State
start info trials =
    let
        relatedInfos =
            Dict.get taskId (ExperimentInfo.toDict info) |> Result.fromMaybe ("I couldn't fetch the value associated with: " ++ taskId)

        nextTrial =
            trials
                |> List.filter .isTraining
                |> List.head
    in
    case nextTrial of
        Just x ->
            Logic.startIntro relatedInfos
                (List.filter .isTraining trials)
                (List.filter (not << .isTraining) trials)
                { initState | userAnswer = x.writtenWord, scrambledLetter = toItems x.writtenWord }

        Nothing ->
            Logic.Err "I tried to initate the state with the first trial but I couldn't find a first trial. Please report this error."


toItems : String -> List KeyedItem
toItems string =
    string
        |> String.toList
        |> List.map String.fromChar
        |> toKeyedItem


toKeyedItem : List String -> List ( String, String )
toKeyedItem letters =
    List.map (\( lett, rec ) -> ( "key-" ++ lett ++ String.fromInt rec, lett )) (dedupe letters)


taskId =
    "recSL8cthViyXRx8u"


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
        , resolver = Http.stringResolver <| Data.handleJsonResponse <| decodeTranslationInput
        , timeout = Just 5000
        }


type Msg
    = UserDragsLetter DnDList.Msg
    | PlayAudio String
    | UserClickedFeedbackButton
    | UserClickedNextTrial (Maybe Trial)
    | UserClickedStartMainloop (List Trial)
    | UserClickedSaveData
    | ServerRespondedWithLastRecords (Result Http.Error (List ()))
    | UserClickedStartTraining
    | UserClickedStartAudio String


update msg model =
    let
        currentScrabbleState =
            case Logic.getState model.scrabbleTask of
                Just x ->
                    x

                _ ->
                    initState
    in
    case msg of
        UserDragsLetter dndmsg ->
            let
                ( dnd, items ) =
                    system.update dndmsg model.dnd currentScrabbleState.scrambledLetter
            in
            ( { model
                | dnd = dnd
                , scrabbleTask = Logic.update { currentScrabbleState | scrambledLetter = items, userAnswer = String.concat (List.map Tuple.second items) } model.scrabbleTask
              }
            , system.commands dnd
            )

        PlayAudio url ->
            ( model, Ports.playAudio url )

        UserClickedFeedbackButton ->
            ( { model | scrabbleTask = Logic.toggle model.scrabbleTask }, Cmd.none )

        UserClickedNextTrial (Just nextTrial) ->
            ( { model
                | scrabbleTask =
                    Logic.next
                        { currentScrabbleState
                            | userAnswer = nextTrial.writtenWord
                            , scrambledLetter = toItems nextTrial.writtenWord
                            , remainingListenings = 3
                        }
                        model.scrabbleTask
              }
            , Cmd.none
            )

        UserClickedNextTrial Nothing ->
            ( { model
                | scrabbleTask =
                    Logic.next currentScrabbleState model.scrabbleTask
              }
            , Cmd.none
            )

        UserClickedSaveData ->
            let
                responseHandler =
                    ServerRespondedWithLastRecords
            in
            ( model, Logic.saveData responseHandler model.user taskId model.scrabbleTask )

        ServerRespondedWithLastRecords _ ->
            ( model, Cmd.none )

        UserClickedStartMainloop trials ->
            case trials of
                [] ->
                    ( { model | scrabbleTask = Logic.Err "You gave no trial to start the main loop. Please report this error message." }, Cmd.none )

                x :: _ ->
                    ( { model | scrabbleTask = Logic.startMain model.scrabbleTask { currentScrabbleState | userAnswer = x.writtenWord, scrambledLetter = toItems x.writtenWord, remainingListenings = 3 } }, Cmd.none )

        UserClickedStartTraining ->
            ( { model | scrabbleTask = Logic.startTraining model.scrabbleTask }, Cmd.none )

        UserClickedStartAudio url ->
            ( { model | scrabbleTask = Logic.update { currentScrabbleState | remainingListenings = currentScrabbleState.remainingListenings - 1 } model.scrabbleTask }, Ports.playAudio url )


viewScrabbleTask : { a | scrabbleTask : Logic.Task Trial State, dnd : DnDList.Model } -> Html.Styled.Html Msg
viewScrabbleTask model =
    let
        viewLetters scrambledLetters =
            scrambledLetters
                |> List.indexedMap (itemView model.dnd)
                |> Keyed.node "div" containerStyles

        audioButton url =
            div
                [ Html.Styled.Events.onClick (PlayAudio url), class "col-start-2 col-span-4 h-8 w-8" ]
                [ fromUnstyled <| Icons.music ]
    in
    case model.scrabbleTask of
        Logic.NotStarted ->
            text "Not Asked"

        Logic.Running Logic.Instructions data ->
            View.instructions data.infos.instructions UserClickedStartTraining

        Logic.Running Logic.Main data ->
            case data.current of
                Just currentTrial ->
                    div [ class "flex flex-col items-center justify-center" ]
                        [ View.tooltip data.infos.instructions_short
                        , Progressbar.progressBar data.history data.mainTrials
                        , viewAudioButton data.state.remainingListenings currentTrial.audioWord.url
                        , if not data.feedback then
                            div [ class "flex flex-col items-center w-full" ]
                                [ viewLetters data.state.scrambledLetter
                                , ghostView model.dnd
                                    data.state.scrambledLetter
                                ]

                          else
                            div [] []
                        , View.genericSingleChoiceFeedback
                            { isVisible = data.feedback
                            , userAnswer = data.state.userAnswer
                            , target = currentTrial.target
                            , feedback_Correct =
                                ( data.infos.feedback_correct
                                , [ View.bold currentTrial.target ]
                                )
                            , feedback_Incorrect = ( data.infos.feedback_incorrect, [ View.bold currentTrial.target ] )
                            , button = View.navigationButton UserClickedFeedbackButton (UserClickedNextTrial data.next) data.feedback data.state.userAnswer
                            }
                        ]

                Nothing ->
                    View.end data.infos.end UserClickedSaveData "context-understanding"

        Logic.Running Logic.Training data ->
            case data.current of
                Just currentTrial ->
                    div [ class "flex flex-col items-center" ]
                        [ viewAudioButton data.state.remainingListenings currentTrial.audioWord.url
                        , viewLetters data.state.scrambledLetter
                        , ghostView model.dnd data.state.scrambledLetter
                        , View.genericSingleChoiceFeedback
                            { isVisible = data.feedback
                            , userAnswer = data.state.userAnswer
                            , target = currentTrial.target
                            , feedback_Correct =
                                ( data.infos.feedback_correct
                                , [ View.bold currentTrial.target ]
                                )
                            , feedback_Incorrect = ( data.infos.feedback_incorrect, [ View.bold currentTrial.target ] )
                            , button = View.navigationButton UserClickedFeedbackButton (UserClickedNextTrial data.next) data.feedback data.state.userAnswer
                            }
                        ]

                Nothing ->
                    View.introToMain (UserClickedStartMainloop data.mainTrials)

        Logic.Loading ->
            text "Loading..."

        Logic.Err reason ->
            text reason


viewAudioButton nTimes url =
    case nTimes of
        3 ->
            View.audioButton UserClickedStartAudio url "Listen"

        2 ->
            View.audioButton UserClickedStartAudio url "Listen again?"

        1 ->
            View.audioButton UserClickedStartAudio url "Listen for the last time?"

        _ ->
            div [] []


itemView : DnDList.Model -> Int -> KeyedItem -> ( String, Html.Styled.Html Msg )
itemView dnd index ( key, item ) =
    let
        itemId : String
        itemId =
            "id-" ++ key
    in
    case system.info dnd of
        Just { dragIndex } ->
            if dragIndex /= index then
                ( key
                , div
                    (Html.Styled.Attributes.id itemId
                        :: itemStyles yellow
                        ++ List.map Html.Styled.Attributes.fromUnstyled (system.dropEvents index itemId)
                    )
                    [ text (String.toUpper item) ]
                )

            else
                ( key
                , div
                    (Html.Styled.Attributes.id itemId :: itemStyles "dimgray")
                    []
                )

        Nothing ->
            ( key
            , div
                (Html.Styled.Attributes.id itemId
                    :: itemStyles yellow
                    ++ List.map Html.Styled.Attributes.fromUnstyled (system.dragEvents index itemId)
                )
                [ text (String.toUpper item) ]
            )


ghostView : DnDList.Model -> List KeyedItem -> Html.Styled.Html Msg
ghostView dnd items =
    let
        maybeDragItem : Maybe KeyedItem
        maybeDragItem =
            system.info dnd
                |> Maybe.andThen
                    (\{ dragIndex } ->
                        items
                            |> List.drop dragIndex
                            |> List.head
                    )
    in
    case maybeDragItem of
        Just ( _, item ) ->
            div (itemStyles ghostYellow ++ List.map Html.Styled.Attributes.fromUnstyled (system.ghostStyles dnd)) [ text (String.toUpper item) ]

        Nothing ->
            text ""


subscriptions model =
    system.subscriptions model.dnd



--DATA
-- SYSTEM


config : DnDList.Config KeyedItem
config =
    { beforeUpdate = \_ _ list -> list
    , movement = DnDList.Free
    , listen = DnDList.OnDrag
    , operation =
        DnDList.Swap
    }


system : DnDList.System KeyedItem Msg
system =
    DnDList.create config UserDragsLetter



-- STYLES


yellow : String
yellow =
    "#ffef33"


ghostYellow : String
ghostYellow =
    "#fcf279"


containerStyles : List (Html.Styled.Attribute msg)
containerStyles =
    [ Html.Styled.Attributes.style "display" "flex"
    , Html.Styled.Attributes.style "flex-wrap" "wrap"
    , Html.Styled.Attributes.style "align-items" "center"

    --, Html.Styled.Attributes.style "justify-content" "center"
    , Html.Styled.Attributes.style "padding-top" "2em"
    , class "font-bold text-xl"
    ]


itemStyles : String -> List (Html.Styled.Attribute msg)
itemStyles color =
    [ Html.Styled.Attributes.style "width" "5rem"
    , Html.Styled.Attributes.style "height" "5rem"
    , Html.Styled.Attributes.style "background-color" color
    , Html.Styled.Attributes.style "border-radius" "8px"
    , Html.Styled.Attributes.style "color" "black"
    , Html.Styled.Attributes.style "cursor" "pointer"
    , Html.Styled.Attributes.style "margin" "0 2em 2em 0"
    , Html.Styled.Attributes.style "display" "flex"
    , Html.Styled.Attributes.style "align-items" "center"
    , Html.Styled.Attributes.style "justify-content" "center"
    ]
