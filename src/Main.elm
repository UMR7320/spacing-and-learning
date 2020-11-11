module Main exposing (main)

import Browser
import Browser.Navigation as Nav
import Data
import Experiment.Experiment as E
import Experiment.Meaning as Meaning exposing (..)
import Html.Styled exposing (..)
import Html.Styled.Attributes exposing (attribute, class, disabled, href)
import Html.Styled.Events exposing (onClick)
import Http
import RemoteData exposing (RemoteData, WebData)
import Route exposing (Route(..))
import Url exposing (Url)
import Url.Builder
import View exposing (navIn, navOut)


type alias Flags =
    {}



-- MODEL


type alias Model =
    { key : Nav.Key
    , route : Route.Route
    , experiment : E.Experiment Meaning.Trial Meaning.State
    }


main : Program Flags Model Msg
main =
    Browser.application
        { init = init
        , onUrlRequest = UserClickedLink
        , onUrlChange = BrowserChangedUrl
        , view = view
        , update = update
        , subscriptions = subscriptions
        }



-- VIEW


project : { description : String, title : String, url : String }
project =
    { title = "Apprentissage et espacement"
    , description = """
        Une expérience visant à mieux comprendre l'acquisition de nouvelles structures grammaticales en langue anglaise. 
      """
    , url = Url.Builder.absolute [ "start" ] []
    }


view : Model -> Browser.Document Msg
view model =
    { title = project.title
    , body = [ body model |> div [] |> toUnstyled ]
    }


body : Model -> List (Html Msg)
body model =
    [ View.header
        [ navIn "L'expérience" "/start"
        , navOut "BCL" "https://bcl.cnrs.fr/"
        , navOut "L'équipe" "https://bcl.cnrs.fr/rubrique225"
        ]
    , View.container <|
        case model.route of
            ExperimentStart ->
                viewExperiment model

            NotFound ->
                View.notFound
    ]


viewExperiment : Model -> List (Html Msg)
viewExperiment model =
    let
        content attributes =
            ul <|
                [ attribute "data-test" "package"
                , class "mt-8 flex flex-col justify-center max-w-2xl h-48"
                , class "bg-transition rounded"
                , class "pl-10 font-semibold leading-loose"
                ]
                    ++ attributes

        item key value =
            li [] (View.keyValue key value)

        startButton =
            button
                [ class "w-64"
                , attribute "data-action" "start-experiment"
                , onClick UserClickedStartExperimentButton
                ]
                [ text "Commencer l'expérience" ]
    in
    case model.experiment of
        E.NotAsked ->
            [ h1 [] [ text "Apprentissage et Espacement" ]

            --    , h2 [] [ text "Serverless Lambda function on Netlify" ]
            , p
                [ class "max-w-xl text-xl mb-8" ]
                [ text "Une expérience visant à mieux comprendre l'acquisition de nouvelles structures grammaticales en langue anglaise. "
                ]
            , div [] [ startButton ]
            ]

        E.Loading ->
            [ h1 [] [ text "Apprentissage et Espacement" ]
            , p
                [ class "max-w-xl text-xl mb-8" ]
                [ text "Une expérience visant à mieux comprendre l'acquisition de nouvelles structures grammaticales en langue anglaise. "
                ]
            , div []
                [ button [ class "w-56 cursor-wait", disabled True ]
                    [ text "Loading..." ]
                ]
            ]

        E.Failure reason ->
            [ h1 [] [ text "Apprentissage et Espacement" ]
            , p
                [ class "max-w-xl text-xl mb-8" ]
                [ text "Une expérience visant à mieux comprendre l'acquisition de nouvelles structures grammaticales en langue anglaise. "
                ]
            , content
                [ attribute "data-result" "error"
                , class "bg-red-500 p-12 text-white text-center"
                ]
                [ li [] [ text (buildErrorMessage reason) ] ]
            ]

        (E.Ready ( block, step )) as exp ->
            case block of
                E.Meaning input output ->
                    let
                        trial =
                            Meaning.getTrial_ exp
                    in
                    Meaning.view
                        exp
                        [ View.radio
                            trial.option1
                            (output.userAnswer == trial.option1)
                            (UserClickedRadioButton trial.option1)
                        , View.radio
                            trial.option2
                            (output.userAnswer == trial.option2)
                            (UserClickedRadioButton trial.option2)
                        , View.radio
                            trial.option3
                            (output.userAnswer == trial.option3)
                            (UserClickedRadioButton trial.option3)
                        ]
                        UserClickedFeedbackButton
                        UserClickedNextTrialButton

                E.Translation ->

                E.Synonym ->
                    [ text "I didn't create this part yet" ]

                E.ClosedChoiceSpelling ->
                    [ text "I didn't create this part yet" ]

                E.ScrabbleSpelling ->
                    [ text "I didn't create this part yet" ]

                E.FreeWritingSpelling ->
                    [ text "I didn't create this part yet" ]

                E.ClosedChoiceTextCompletion ->
                    [ text "I didn't create this part yet" ]

                E.ClosedChoiceTextAndAudioUnderstanding ->
                    [ text "I didn't create this part yet" ]

                E.FreeWritingTextCompletion ->
                    [ text "I didn't create this part yet" ]



-- UPDATE


type Msg
    = BrowserChangedUrl Url
    | UserClickedLink Browser.UrlRequest
    | UserClickedStartExperimentButton
    | ServerRespondedWithMeaningInput (Result Http.Error (List Meaning.Trial))
    | UserClickedRadioButton String
    | UserClickedFeedbackButton
    | UserClickedNextTrialButton


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    let
        currentState =
            Meaning.getState model.experiment
    in
    case msg of
        BrowserChangedUrl url ->
            ( { model | route = Route.fromUrl url }
            , Cmd.none
            )

        UserClickedLink urlRequest ->
            case urlRequest of
                Browser.Internal url ->
                    ( model
                    , Nav.pushUrl model.key (Url.toString url)
                    )

                Browser.External url ->
                    ( model
                    , Nav.load url
                    )

        UserClickedStartExperimentButton ->
            ( { model | experiment = E.Loading }
            , Http.get
                { url =
                    buildQuery
                        { app = apps.spacing
                        , base = "input"
                        , view_ = "Meaning"
                        }
                , expect =
                    Http.expectJson
                        ServerRespondedWithMeaningInput
                        Meaning.decodeMeaningInput
                }
            )

        ServerRespondedWithMeaningInput (Result.Ok data) ->
            ( { model | experiment = E.Ready ( E.Meaning data Meaning.initState, E.MainLoop 0 False ) }, Cmd.none )

        ServerRespondedWithMeaningInput (Result.Err reason) ->
            ( { model | experiment = E.Failure reason }, Cmd.none )

        UserClickedRadioButton newChoice ->
            ( { model | experiment = model.experiment |> Meaning.updateState { currentState | userAnswer = newChoice } }
            , Cmd.none
            )

        UserClickedFeedbackButton ->
            ( { model | experiment = model.experiment |> E.toggleFeedback }, Cmd.none )

        UserClickedNextTrialButton ->
            ( { model | experiment = model.experiment |> E.nextTrial |> E.toggleFeedback }, Cmd.none )


init : Flags -> Url -> Nav.Key -> ( Model, Cmd Msg )
init flags url key =
    let
        route =
            Route.fromUrl url
    in
    ( { key = key
      , route = route
      , experiment = E.NotAsked
      }
    , Cmd.none
    )


subscriptions : Model -> Sub Msg
subscriptions model =
    Sub.none


buildErrorMessage : Http.Error -> String
buildErrorMessage httpError =
    case httpError of
        Http.BadUrl message ->
            message

        Http.Timeout ->
            "Server is taking too long to respond. Please try again later."

        Http.NetworkError ->
            "Unable to reach server."

        Http.BadStatus statusCode ->
            "Request failed with status code: " ++ String.fromInt statusCode

        Http.BadBody message ->
            message


buildQuery : AirtableQueryParameters -> String
buildQuery { app, base, view_ } =
    Url.Builder.absolute
        [ ".netlify"
        , "functions"
        , "api"
        ]
        [ Url.Builder.string "app" app
        , Url.Builder.string "base" base
        , Url.Builder.string "view" view_
        ]


apps : { spacing : String, sleep : String }
apps =
    { spacing = "appvKOc8FH0j48Hw1"
    , sleep = "appTEVHZLw3jNa7fU"
    }


type alias AirtableQueryParameters =
    { app : String
    , base : String
    , view_ : String
    }
