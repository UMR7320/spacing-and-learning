module Session2.Spelling2 exposing (..)

import Activity exposing (Activity)
import ActivityInfo exposing (ActivityInfo, Session(..))
import Data
import DnDList
import Html.Styled exposing (..)
import Html.Styled.Attributes exposing (class)
import Html.Styled.Keyed as Keyed
import Http
import Json.Decode as Decode exposing (Decoder, string)
import Json.Decode.Pipeline exposing (..)
import Json.Encode as Encode
import Ports
import Random exposing (Generator)
import Random.Extra
import Random.List
import RemoteData exposing (RemoteData)
import Route exposing (Session2Activity(..))
import Session2.Meaning2 exposing (Msg(..), decodeTrials)
import Task
import Time
import Url.Builder
import View



-- MODEL


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
    , step : Step
    }


type alias Spelling2 =
    Activity Trial State


type Step
    = ListeningFirstTime
    | Answering


type alias Item =
    String


type alias KeyedItem =
    ( String, Item )


type alias Model superModel =
    { superModel
        | spelling2 : Spelling2
        , user : Maybe String
        , dnd : DnDList.Model
    }


initState : State
initState =
    State "DefaultUid" "" [] 3 ListeningFirstTime


defaultTrial : Trial
defaultTrial =
    Trial "defaultTrial" "defaultTrial" (Data.AudioFile "" "") False ""


infoLoaded : List ActivityInfo -> Spelling2 -> Spelling2
infoLoaded infos =
    Activity.infoLoaded
        Session2
        "Spelling 2"
        infos
        initState


init : String -> Model a -> ( Model a, Cmd Msg )
init group model =
    ( { model | spelling2 = Activity.loading }
    , Cmd.batch
        [ getRecords group
        , Ports.enableAlertOnExit ()
        ]
    )



--VIEW


viewScrabbleActivity : { a | spelling2 : Activity.Activity Trial State, dnd : DnDList.Model } -> Html.Styled.Html Msg
viewScrabbleActivity model =
    let
        viewLetters scrambledLetters =
            scrambledLetters
                |> List.indexedMap (itemView model.dnd)
                |> Keyed.node "div" (containerStyles (List.length scrambledLetters))
    in
    case model.spelling2 of
        Activity.NotStarted ->
            text "Not Asked"

        Activity.Running Activity.Instructions data ->
            View.instructions data.infos UserClickedStartTraining

        Activity.Running Activity.Main data ->
            case data.current of
                Just currentTrial ->
                    div [ class "flex flex-col items-center justify-center" ]
                        [ viewAudioButton data.state.remainingListenings currentTrial.audioWord.url
                        , if data.state.step == ListeningFirstTime then
                            div [] []

                          else if not data.feedback && data.state.step == Answering then
                            div [ class "flex flex-col items-center w-full" ]
                                [ viewLetters data.state.scrambledLetter
                                , ghostView model.dnd
                                    data.state.scrambledLetter
                                ]

                          else
                            data.state.userAnswer
                                |> String.toList
                                |> List.map (\item -> div (itemStyles yellow) [ text ((String.toUpper << String.fromChar) item) ])
                                |> div (containerStyles (String.length data.state.userAnswer))
                        , View.genericSingleChoiceFeedback
                            { isVisible = data.feedback
                            , userAnswer = data.state.userAnswer
                            , target = currentTrial.target
                            , feedback_Correct =
                                ( data.infos.feedback_correct
                                , [ View.bold currentTrial.target ]
                                )
                            , feedback_Incorrect = ( data.infos.feedback_incorrect, [ View.bold currentTrial.target ] )
                            , button =
                                View.navigationButton UserClickedFeedbackButton
                                    (UserClickedNextTrial data.next)
                                    data.feedback
                                    (if data.state.step == ListeningFirstTime then
                                        ""

                                     else
                                        data.state.userAnswer
                                    )
                            }
                        ]

                Nothing ->
                    View.end data.infos.end UserClickedSaveData (Just "context-understanding")

        Activity.Running Activity.Training data ->
            case data.current of
                Just currentTrial ->
                    div [ class "flex flex-col items-center" ]
                        [ viewAudioButton data.state.remainingListenings currentTrial.audioWord.url
                        , if data.state.step == ListeningFirstTime then
                            div [] []

                          else if not data.feedback && data.state.step == Answering then
                            div [ class "flex flex-col items-center w-full" ]
                                [ viewLetters data.state.scrambledLetter
                                , ghostView model.dnd
                                    data.state.scrambledLetter
                                ]

                          else
                            data.state.userAnswer
                                |> String.toList
                                |> List.map (\item -> div (itemStyles yellow) [ text ((String.toUpper << String.fromChar) item) ])
                                |> div (containerStyles (String.length data.state.userAnswer))
                        , View.genericSingleChoiceFeedback
                            { isVisible = data.feedback
                            , userAnswer = data.state.userAnswer
                            , target = currentTrial.target
                            , feedback_Correct =
                                ( data.infos.feedback_correct
                                , [ View.bold currentTrial.target ]
                                )
                            , feedback_Incorrect = ( data.infos.feedback_incorrect, [ View.bold currentTrial.target ] )
                            , button =
                                View.navigationButton UserClickedFeedbackButton
                                    (UserClickedNextTrial data.next)
                                    data.feedback
                                    (if data.state.step == ListeningFirstTime then
                                        ""

                                     else
                                        data.state.userAnswer
                                    )
                            }
                        ]

                Nothing ->
                    View.introToMain (UserClickedStartMainloop data.mainTrials)

        Activity.Loading _ _ ->
            View.loading

        Activity.Err reason ->
            text reason


viewAudioButton : Int -> String -> Html Msg
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



-- UPDATE


type Msg
    = GotTrials (RemoteData Http.Error (List Trial))
    | GotRandomizedTrials (List Trial)
    | UserDragsLetter DnDList.Msg
    | PlayAudio String
    | UserClickedFeedbackButton
    | UserClickedNextTrial (Maybe Trial)
    | NextTrial (Maybe Trial) Time.Posix
    | UserClickedStartMainloop (List Trial)
    | UserClickedSaveData
    | UserClickedStartTraining
    | UserClickedStartAudio String
    | AudioEnded { eventType : String, name : String, timestamp : Int }
    | HistoryWasSaved (Result Http.Error String)


update : Msg -> Model a -> ( Model a, Cmd Msg )
update msg model =
    let
        currentScrabbleState =
            case Activity.getState model.spelling2 of
                Just x ->
                    x

                _ ->
                    initState
    in
    case msg of
        GotTrials (RemoteData.Success trials) ->
            ( model
            , Random.generate GotRandomizedTrials (shuffleTrials trials)
            )

        GotRandomizedTrials trials ->
            ( { model | spelling2 = Activity.trialsLoaded trials initState model.spelling2 }
            , Cmd.none
            )

        GotTrials (RemoteData.Failure error) ->
            ( { model
                | spelling2 = Activity.Err (Data.buildErrorMessage error)
              }
            , Cmd.none
            )

        GotTrials _ ->
            ( model, Cmd.none )

        UserDragsLetter dndmsg ->
            let
                ( dnd, items ) =
                    system.update dndmsg model.dnd currentScrabbleState.scrambledLetter
            in
            ( { model
                | dnd = dnd
                , spelling2 = Activity.update { currentScrabbleState | scrambledLetter = items, userAnswer = String.concat (List.map Tuple.second items) } model.spelling2
              }
            , system.commands dnd
            )

        PlayAudio url ->
            ( model, Ports.playAudio url )

        UserClickedFeedbackButton ->
            ( { model | spelling2 = Activity.toggle model.spelling2 }, Cmd.none )

        UserClickedNextTrial maybeNextTrial ->
            ( model, Task.perform (NextTrial maybeNextTrial) Time.now )

        NextTrial maybeNextTrial timestamp ->
            let
                spelling2 =
                    case maybeNextTrial of
                        Just nextTrial ->
                            Activity.next
                                timestamp
                                { currentScrabbleState
                                    | userAnswer = nextTrial.writtenWord
                                    , scrambledLetter = toItems nextTrial.writtenWord
                                    , remainingListenings = 3
                                    , step = ListeningFirstTime
                                }
                                model.spelling2

                        Nothing ->
                            Activity.next timestamp currentScrabbleState model.spelling2

                newModel =
                    { model | spelling2 = spelling2 }
            in
            ( newModel
            , saveData newModel
            )

        -- data is now saved after each "trial", so this does nothing and shoud be removed
        UserClickedSaveData ->
            ( model, Cmd.none )

        HistoryWasSaved _ ->
            ( model, Cmd.none )

        UserClickedStartMainloop trials ->
            case trials of
                [] ->
                    ( { model | spelling2 = Activity.Err "You gave no trial to start the main loop. Please report this error message." }, Cmd.none )

                x :: _ ->
                    ( { model | spelling2 = Activity.startMain model.spelling2 { currentScrabbleState | userAnswer = x.writtenWord, scrambledLetter = toItems x.writtenWord, remainingListenings = 3, step = ListeningFirstTime } }, Cmd.none )

        UserClickedStartTraining ->
            ( { model | spelling2 = Activity.startTraining model.spelling2 }, Cmd.none )

        UserClickedStartAudio url ->
            ( { model | spelling2 = Activity.update { currentScrabbleState | remainingListenings = currentScrabbleState.remainingListenings - 1 } model.spelling2 }, Ports.playAudio url )

        AudioEnded { eventType } ->
            case eventType of
                "SoundEnded" ->
                    ( { model | spelling2 = Activity.update { currentScrabbleState | step = Answering } model.spelling2 }, Cmd.none )

                _ ->
                    ( model, Cmd.none )


toItems : String -> List KeyedItem
toItems string =
    string
        |> String.toList
        |> List.map String.fromChar
        |> toKeyedItem


toKeyedItem : List String -> List ( String, String )
toKeyedItem letters =
    List.map (\( lett, rec ) -> ( "key-" ++ lett ++ String.fromInt rec, lett )) (dedupe letters)


shuffleWordLetters : String -> Generator String
shuffleWordLetters word =
    word
        |> String.toList
        |> Random.List.shuffle
        |> Random.map String.fromList


shuffleTrialWord : Trial -> Generator Trial
shuffleTrialWord trial =
    trial.writtenWord
        |> shuffleWordLetters
        |> Random.map (\shuffledWord -> { trial | writtenWord = shuffledWord })


shuffleTrials : List Trial -> Generator (List Trial)
shuffleTrials trials =
    trials
        |> Random.Extra.traverse shuffleTrialWord
        |> Random.andThen Random.List.shuffle



-- SUBSCRIPTIONS


subscriptions : Model a -> Sub Msg
subscriptions model =
    case model.spelling2 of
        Activity.Running _ { state } ->
            case state.step of
                ListeningFirstTime ->
                    Sub.batch [ Ports.audioEnded AudioEnded ]

                _ ->
                    system.subscriptions model.dnd

        _ ->
            Sub.none



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


containerStyles : Int -> List (Html.Styled.Attribute msg)
containerStyles lettersCount =
    [ Html.Styled.Attributes.style "display" "grid"
    , Html.Styled.Attributes.style "grid-template-columns" ("repeat(" ++ String.fromInt lettersCount ++ ", 1fr)")
    , Html.Styled.Attributes.style "grid-gap" "3rem"
    , Html.Styled.Attributes.style "justify-content" "center"
    , class "font-bold text-2xl w-full mt-12 mb-8"
    ]


itemStyles : String -> List (Html.Styled.Attribute msg)
itemStyles color =
    [ Html.Styled.Attributes.style "width" "5rem"
    , Html.Styled.Attributes.style "height" "5rem"
    , Html.Styled.Attributes.style "background-color" color
    , Html.Styled.Attributes.style "border-radius" "8px"
    , Html.Styled.Attributes.style "color" "black"
    , Html.Styled.Attributes.style "cursor" "pointer"
    , Html.Styled.Attributes.style "display" "flex"
    , Html.Styled.Attributes.style "align-items" "center"
    , Html.Styled.Attributes.style "justify-content" "center"
    ]



-- HTTP


getRecords : String -> Cmd Msg
getRecords group =
    Http.get
        { url =
            Url.Builder.absolute [ ".netlify", "functions", "api" ]
                [ Url.Builder.string "base" "input"
                , Url.Builder.string "view" "Meaning"
                , Url.Builder.string "filterByFormula" ("{Classe} = \"" ++ group ++ "\"")
                ]
        , expect = Http.expectJson (RemoteData.fromResult >> GotTrials) decodeTrials
        }


decodeTrials : Decoder (List Trial)
decodeTrials =
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


saveData : Model a -> Cmd Msg
saveData model =
    let
        history =
            Activity.getHistory model.spelling2
                |> List.filter (\( trial, _, _ ) -> not trial.isTraining)

        userId =
            model.user |> Maybe.withDefault "recd18l2IBRQNI05y"

        payload =
            updateHistoryEncoder userId history
    in
    Http.request
        { method = "PATCH"
        , headers = []
        , url = Data.buildQuery { app = Data.apps.spacing, base = "users", view_ = "Session2_output" }
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
        [ ( "Spelling2", Encode.string (Encode.encode 0 (Encode.list historyItemEncoder history)) )
        ]


historyItemEncoder : ( Trial, State, Time.Posix ) -> Encode.Value
historyItemEncoder ( { uid, target }, { userAnswer }, timestamp ) =
    Encode.object
        [ ( "trialUid", Encode.string uid )
        , ( "target", Encode.string target )
        , ( "answer", Encode.string userAnswer )
        , ( "answeredAt", Encode.int (Time.posixToMillis timestamp) )
        ]



-- INTERNALS


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
