module Postest.CloudWords exposing (CloudWords(..), Msg(..), Word, WordKnowledge, toggle, update, view, words)

import Data
import Dict
import Html.Styled exposing (div, h1, input, label, span, text)
import Html.Styled.Attributes exposing (class, type_)
import Html.Styled.Events
import Http
import Json.Decode as Decode
import Json.Encode as Encode
import Session
import Session1.Session as Session1
import Session2.Session as Session2
import Session3.Session as Session3
import View


type WordKnowledge
    = Known
    | NotKnown
    | MaybeKnown


toggle : comparable -> Dict.Dict comparable Word -> Dict.Dict comparable Word
toggle key =
    Dict.update key
        (\old ->
            case old of
                Just value ->
                    case value.knowledge of
                        NotKnown ->
                            Just { value | knowledge = MaybeKnown }

                        MaybeKnown ->
                            Just { value | knowledge = Known }

                        Known ->
                            Just { value | knowledge = NotKnown }

                Nothing ->
                    Nothing
        )


type alias Word =
    { word : String
    , knowledge : WordKnowledge
    }


words : List ( String, { word : String, knowledge : WordKnowledge } )
words =
    [ ( "recCWc45cPRRz6k4k", { word = "crave", knowledge = NotKnown } )
    , ( "recJxzByUxGCTz7bC", { word = "curb", knowledge = NotKnown } )
    , ( "reclQIspVCkKaQ93J", { word = "dazzle", knowledge = NotKnown } )
    , ( "recnrmevKdqgEiRX5", { word = "divert", knowledge = NotKnown } )
    , ( "recJbufDTI1F4ps0i", { word = "dread", knowledge = NotKnown } )
    , ( "recvjrjv0o2uwTXDt", { word = "equate", knowledge = NotKnown } )
    , ( "recIYnug7zkWW2sKj", { word = "hail", knowledge = NotKnown } )
    , ( "recDHT4TX9gtv9Jnn", { word = "hinder", knowledge = NotKnown } )
    , ( "recXOzqJwXk5la1eu", { word = "hum", knowledge = NotKnown } )
    , ( "recpqxWfX5CGMrFkx", { word = "incur", knowledge = NotKnown } )
    , ( "recfFOa9cCagckZ5Z", { word = "intrude", knowledge = NotKnown } )
    , ( "rec7hosYCJdQ23AdG", { word = "juggle", knowledge = NotKnown } )
    , ( "recPdvyZoRGIZcA57", { word = "loathe", knowledge = NotKnown } )
    , ( "recvUCQzxkhAffUKo", { word = "mingle", knowledge = NotKnown } )
    , ( "recI5YFzd7aMRs1Gg", { word = "moan", knowledge = NotKnown } )
    , ( "recl26ebnJ5zOfvxO", { word = "pinpoint", knowledge = NotKnown } )
    , ( "recmjaHTHFXrBGQSP", { word = "ponder", knowledge = NotKnown } )
    , ( "recAwJcgy9898scfb", { word = "refrain", knowledge = NotKnown } )
    , ( "reckyzxJrCqcbzg9p", { word = "relish", knowledge = NotKnown } )
    , ( "reckIHkM3qeSAe8uE", { word = "saddle", knowledge = NotKnown } )
    , ( "recTQ4T1A7OQEI3hN", { word = "seduce", knowledge = NotKnown } )
    , ( "recb1amYzKr35RYfW", { word = "sprinkle", knowledge = NotKnown } )
    , ( "recUiB0O0W8Lwyymv", { word = "strive", knowledge = NotKnown } )
    , ( "recXkLmHDF43nt14L", { word = "uphold", knowledge = NotKnown } )
    , ( "recK58CFOied5QbWY", { word = "vow", knowledge = NotKnown } )
    , ( "reci13I4WD6BL2aYe", { word = "wield", knowledge = NotKnown } )
    , ( "recMzEWiaVwdBxAsS", { word = "withstand", knowledge = NotKnown } )

    --, ( "recwMm0FnWOye0WIp", { word = "spell", knowledge = NotKnown } )
    --, ( "recXMPUEcjlZ4mN8x", { word = "focus", knowledge = NotKnown } )
    ]


type Msg
    = UserToggledInCloudWords String
    | UserClickedSaveData Payload
    | ServerRespondedWithUpdatedUser (Result Http.Error String)


update : Msg -> Model superModel -> ( Model superModel, Cmd Msg )
update msg model =
    case msg of
        UserToggledInCloudWords word ->
            case model.cloudWords of
                Running w ->
                    ( { model | cloudWords = Running (toggle word w) }, Cmd.none )

                End ->
                    ( model, Cmd.none )

        UserClickedSaveData ({ knownWords, maybeKnownWords, unknownWords } as result) ->
            let
                userId =
                    model.user |> Maybe.withDefault "recd18l2IBRQNI05y"

                session =
                    if model.session1 /= Session.NotAsked then
                        "S1"

                    else if model.session2 /= Session.NotAsked then
                        "S2"

                    else
                        "S3"

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
            ( model, updateVocabularyScore (Http.jsonBody encode) ServerRespondedWithUpdatedUser responseDecoder )

        ServerRespondedWithUpdatedUser response ->
            ( { model | cloudWords = End }, Cmd.none )


updateVocabularyScore : Http.Body -> (Result Http.Error a -> msg) -> Decode.Decoder a -> Cmd msg
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


type alias Payload =
    { knownWords : List String, maybeKnownWords : List String, unknownWords : List String }


type alias Model superModel =
    { superModel | cloudWords : CloudWords, user : Maybe String, session1 : Session1.Session1, session2 : Session2.Session2, session3 : Session3.Session3 }


type CloudWords
    = Running (Dict.Dict String Word)
    | End


view : Model superModel -> List (Html.Styled.Html Msg)
view model =
    case model.cloudWords of
        Running w ->
            [ div [ class "grid grid-flow-col content-center grid-rows-4 auto-cols-max gap-4 " ] <|
                (Dict.map
                    (\id value ->
                        div
                            [ class "transition duration-300 ease-in-out rounded-lg cursor-pointer p-2 align-baseline flex flex-row"
                            , class
                                (if value.knowledge == NotKnown then
                                    "bg-red-500"

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
                                    "👎"

                                else if value.knowledge == MaybeKnown then
                                    "🤔"

                                else
                                    "👍"
                            , span [ class "pl-2 text-lg" ] [ text value.word ]
                            ]
                    )
                    w
                    |> Dict.values
                )
            , View.button
                { message =
                    UserClickedSaveData
                        { knownWords = filterWords w Known
                        , maybeKnownWords = filterWords w MaybeKnown
                        , unknownWords = filterWords w NotKnown
                        }
                , txt = "Click here to save data"
                , isDisabled = False
                }
            ]

        End ->
            [ h1 [] [ text "This session is over." ] ]


filterWords w wordknowledge =
    Dict.filter (\k v -> v.knowledge == wordknowledge) w |> Dict.keys
