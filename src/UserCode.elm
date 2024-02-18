module UserCode exposing (..)

import Browser.Navigation exposing (Key, pushUrl)
import Html.Styled exposing (code, div, form, h1, input, p, text)
import Html.Styled.Attributes exposing (autofocus, class, placeholder, style, value)
import Html.Styled.Events exposing (onInput)
import Http
import Json.Decode exposing (Decoder, andThen, bool, field, map, map2, string)
import RemoteData exposing (RemoteData(..))
import Url.Builder



-- MODEL


type alias Model =
    { code : String
    , response : RemoteData Http.Error CodeCheckResponse
    , date : Maybe String
    }


type CodeCheckResponse
    = OK String String
    | KO String


emptyModel : Model
emptyModel =
    { code = ""
    , response = RemoteData.NotAsked
    , date = Nothing
    }



-- VIEW


view : Model -> List (Html.Styled.Html Msg)
view model =
    [ div
        []
        [ h1 [] [ text "Bienvenue 😀" ]
        , div
            [ class "mb-5" ]
            [ p [] [ text "Bonjour et bienvenue sur la plateforme LexLearn Collège." ]
            , p [] [ text "Entre ton code pour commencer l'activité." ]
            ]
        , form
            []
            [ input
                [ class "text-3xl p-4 border-2 border-gray-600 rounded"
                , style "width" "8ch"
                , value model.code
                , autofocus True
                , onInput CodeUpdated
                ]
                []
            ]
        , case model.response of
            Success (KO reason) ->
                div [ class "text-red-600 mt-2" ] [ text reason ]

            Loading ->
                div
                    [ class "mt-2" ]
                    [ text "Chargement" ]

            _ ->
                div [ class "mt-2" ] [ text " " ]
        ]
    ]



-- UPDATE


type Msg
    = CodeUpdated String
    | CodeChecked (RemoteData Http.Error CodeCheckResponse)


update : Msg -> { a | userCode : Model, key : Key } -> ( { a | userCode : Model, key : Key }, Cmd Msg )
update msg model =
    let
        ( newModel, cmd ) =
            subUpdate msg model.userCode model.key
    in
    ( { model | userCode = newModel }
    , cmd
    )


subUpdate : Msg -> Model -> Key -> ( Model, Cmd Msg )
subUpdate msg model key =
    case msg of
        CodeUpdated newCode ->
            if String.length newCode >= 4 then
                ( { model | code = newCode, response = Loading }
                , checkUserCode newCode model.date CodeChecked
                )

            else
                ( { model | code = newCode }
                , Cmd.none
                )

        CodeChecked (Success (OK userId session)) ->
            ( { model | code = "", response = NotAsked }
            , pushUrl key ("/user/" ++ userId ++ "/" ++ session)
            )

        CodeChecked response ->
            ( { model | response = response }
            , Cmd.none
            )


checkUserCode : String -> Maybe String -> (RemoteData Http.Error CodeCheckResponse -> msg) -> Cmd msg
checkUserCode code date msg =
    Http.get
        { url =
            Url.Builder.absolute
                [ ".netlify", "functions", "check-user-code" ]
                ([ Just (Url.Builder.string "code" code)
                 , Maybe.map (Url.Builder.string "date") date
                 ]
                    -- turn a (List (Maybe a)) into a (List a) removing Nothings
                    |> List.filterMap identity
                )
        , expect = Http.expectJson (RemoteData.fromResult >> msg) decodeCheckUserCode
        }


decodeCheckUserCode : Decoder CodeCheckResponse
decodeCheckUserCode =
    field "success" bool
        |> andThen
            (\success ->
                if success then
                    map2 OK
                        (field "userId" string)
                        (field "session" string)

                else
                    map KO (field "reason" string)
            )
