module PostTests.CloudWords exposing (CloudWords(..), Msg(..), Word, WordKnowledge, getWords, toggle, update, view)

import Browser.Navigation exposing (pushUrl)
import Data
import Dict exposing (Dict)
import Html.Styled exposing (code, div, h1, p, pre, span, text)
import Html.Styled.Attributes exposing (class)
import Html.Styled.Events
import Http
import Json.Decode as Decode exposing (..)
import Json.Encode as Encode
import RemoteData
import Session exposing (Session(..))
import Url.Builder
import View



-- MODEL


type alias InputWord =
    { id : String
    , word : String
    , isExperimental : Bool
    }


type WordKnowledge
    = Known
    | NotKnown
    | MaybeKnown


type alias Word =
    { word : String
    , knowledge : WordKnowledge
    }


type alias Model superModel =
    { superModel
        | cloudWords : CloudWords
        , user : Maybe String
        , key : Browser.Navigation.Key
    }


type CloudWords
    = Loading
    | Running (Dict String Word)
    | Error Http.Error
    | End



-- VIEW


view : String -> Model superModel -> List (Html.Styled.Html Msg)
view session model =
    case model.cloudWords of
        Loading ->
            [ text "Loading words..." ]

        Error error ->
            [ div
                [ class "flow" ]
                [ div [] [ text "Failed loading words:" ]
                , pre [ class "overflow-x-scroll" ] [ code [] [ text (Data.buildErrorMessage error) ] ]
                ]
            ]

        Running w ->
            [ div [ class "cloudwords" ]
                [ h1 [] [ text "Progress check" ]
                , div [ class "pb-8 flow" ] [ View.fromMarkdown "How well do you think you know our verbs now?\n\nClick on each verb to change its color:\n\n- grey: I don't know it yet\n\n- white: I know it a little\n\n- green: I know it well" ]
                , div [ class "grid grid-flow-col content-center grid-rows-4 grid-cols-4 gap-4 pb-8 " ] <|
                    (Dict.map
                        (\id value ->
                            div
                                [ class "transition duration-300 ease-in-out rounded-lg cursor-pointer p-2 align-baseline flex flex-row"
                                , class
                                    (if value.knowledge == NotKnown then
                                        "bg-gray-500"

                                     else if value.knowledge == MaybeKnown then
                                        ""

                                     else
                                        "bg-green-500"
                                    )
                                , Html.Styled.Events.onClick <|
                                    UserToggledInCloudWords id
                                ]
                                [ text <|
                                    if value.knowledge == NotKnown then
                                        "🤷\u{200D}♀️"

                                    else if value.knowledge == MaybeKnown then
                                        "🤔"

                                    else
                                        "👍"
                                , span [ class "pl-2" ] [ text value.word ]
                                ]
                        )
                        w
                        |> Dict.values
                    )
                , View.button
                    { message =
                        UserClickedSaveData
                            session
                            { knownWords = filterWords w Known
                            , maybeKnownWords = filterWords w MaybeKnown
                            , unknownWords = filterWords w NotKnown
                            }
                    , txt = "Save"
                    , isDisabled = False
                    }
                ]
            ]

        End ->
            [ div
                []
                [ h1
                    []
                    [ text "This is the end of the session" ]
                , p [] [ text "Check your calendar for your next learning session. You will receive an email with the link on the correct day." ]
                ]
            ]


filterWords : Dict comparable { a | knowledge : b } -> b -> List comparable
filterWords w wordknowledge =
    Dict.filter (\_ v -> v.knowledge == wordknowledge) w |> Dict.keys



-- UPDATE


type alias Payload =
    { knownWords : List String, maybeKnownWords : List String, unknownWords : List String }


type Msg
    = UserToggledInCloudWords String
    | UserClickedSaveData String Payload
    | ServerRespondedWithUpdatedUser String (Result Http.Error String)
    | GotWords (Result Http.Error (List InputWord))


update : Msg -> Model superModel -> ( Model superModel, Cmd Msg )
update msg model =
    case msg of
        GotWords (Result.Err error) ->
            ( { model | cloudWords = Error error }
            , Cmd.none
            )

        GotWords (Result.Ok inputWords) ->
            case model.cloudWords of
                Loading ->
                    ( { model
                        | cloudWords =
                            Running
                                (inputWords
                                    |> List.filter (\word -> word.isExperimental == True)
                                    |> List.map (\word -> ( word.id, Word word.word NotKnown ))
                                    |> Dict.fromList
                                )
                      }
                    , Cmd.none
                    )

                _ ->
                    ( model, Cmd.none )

        UserToggledInCloudWords word ->
            case model.cloudWords of
                Running w ->
                    ( { model | cloudWords = Running (toggle word w) }, Cmd.none )

                _ ->
                    ( model, Cmd.none )

        UserClickedSaveData session result ->
            let
                userId =
                    model.user |> Maybe.withDefault "recd18l2IBRQNI05y"

                encode =
                    Encode.list
                        (\score ->
                            Encode.object
                                [ ( "id", Encode.string userId )
                                , ( "fields"
                                  , Encode.object
                                        [ ( session ++ "_WC_Known", Encode.list Encode.string score.knownWords )
                                        , ( session ++ "_WC_Unkown", Encode.list Encode.string score.unknownWords )
                                        , ( session ++ "_WC_MaybeKnown", Encode.list Encode.string score.maybeKnownWords )
                                        ]
                                  )
                                ]
                        )
                        [ result ]

                responseDecoder =
                    Decode.field "id" Decode.string
            in
            ( model
            , saveWordCloud (Http.jsonBody encode) (ServerRespondedWithUpdatedUser session) responseDecoder
            )

        ServerRespondedWithUpdatedUser session _ ->
            if session == "S3" then
                ( model
                , Browser.Navigation.load "../pretest/vks?version=post"
                )

            else
                ( { model | cloudWords = End }
                , Cmd.none
                )


toggle : comparable -> Dict.Dict comparable Word -> Dict.Dict comparable Word
toggle key =
    Dict.update
        key
        (Maybe.map
            (\value ->
                case value.knowledge of
                    NotKnown ->
                        { value | knowledge = MaybeKnown }

                    MaybeKnown ->
                        { value | knowledge = Known }

                    Known ->
                        { value | knowledge = NotKnown }
            )
        )



-- HTTP


getWords : String -> Cmd Msg
getWords group =
    Http.get
        { url =
            Url.Builder.absolute [ ".netlify", "functions", "api" ]
                [ Url.Builder.string "base" "input"
                , Url.Builder.string "view" "all"
                , Url.Builder.string "filterByFormula" ("{Classe} = \"" ++ group ++ "\"")
                ]
        , expect = Http.expectJson GotWords decodeWords
        }


decodeWords : Decoder (List InputWord)
decodeWords =
    field "records"
        (list
            (map3 InputWord
                (field "id" string)
                (field "Word_Text" string)
                (optionalBool "isExperimental")
            )
        )



-- Airtable is weird and diesn't include boolean properties when they're "false"


optionalBool : String -> Decoder Bool
optionalBool fieldName =
    maybe (field fieldName bool) |> map (Maybe.withDefault False)


saveWordCloud : Http.Body -> (Result Http.Error a -> msg) -> Decoder a -> Cmd msg
saveWordCloud payload callbackMsg decoder =
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
