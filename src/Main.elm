module Main exposing (main)

import Browser
import Browser.Navigation as Nav
import Data
import Experiment.Experiment as E
import Experiment.Meaning as Meaning exposing (..)
import Experiment.Translation as Translation
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


type HigherType
    = None
    | Option1 Meaning.Trial
    | Option2 Translation.Trial


type HigherTypeTwo
    = StateOne Translation.State
    | StateTwo Translation.State


type alias Model =
    { key : Nav.Key
    , route : Route.Route
    , experiment : E.Experiment Meaning.Trial Meaning.State
    , translation : E.Experiment Translation.Trial Translation.Trial
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


init : Flags -> Url -> Nav.Key -> ( Model, Cmd Msg )
init flags url key =
    let
        route =
            Route.fromUrl url
    in
    ( { key = key
      , route = route
      , experiment = E.NotAsked
      , translation = E.NotAsked
      }
    , fetchData route
    )


fetchData : Route -> Cmd Msg
fetchData route =
    case route of
        Meaning ->
            Meaning.getTrialsFromServer ServerRespondedWithMeaningInput

        Translation ->
            Translation.getTrialsFromServer ServerRespondedWithTranslationTrials

        ExperimentStart ->
            Cmd.none

        NotFound ->
            Cmd.none



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
        [ navIn "L'expérience" "/meaning"
        , navOut "BCL" "https://bcl.cnrs.fr/"
        , navOut "L'équipe" "https://bcl.cnrs.fr/rubrique225"
        ]
    , View.container <|
        case model.route of
            ExperimentStart ->
                [ h1 [] [ text "Apprentissage et Espacement" ]
                , p
                    [ class "max-w-xl text-xl mb-8" ]
                    [ text "Une expérience visant à mieux comprendre l'acquisition de nouvelles structures grammaticales en langue anglaise. "
                    ]
                , div [] [ startButton ]
                ]

            Meaning ->
                viewExperiment model

            Translation ->
                [ text "I didn't create this part yet" ]

            NotFound ->
                View.notFound
    ]


startButton : Html Msg
startButton =
    button
        [ class "w-64"
        , attribute "data-action" "start-experiment"
        , onClick UserClickedStartExperimentButton
        ]
        [ text "Commencer l'expérience" ]


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
    in
    case model.experiment of
        E.NotAsked ->
            [ h1 [] [ text "Apprentissage et Espacement" ]
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

        (E.Ready ( E.Meaning trials state, step )) as exp ->
            let
                trial =
                    Meaning.getTrial_ exp

                feedback =
                    Meaning.getFeedbackStatus step

                isCorrect optionN =
                    optionN == trial.definition
            in
            [ Meaning.view
                exp
                [ View.radio
                    trial.option1
                    (state.userAnswer == trial.option1)
                    (isCorrect trial.option1)
                    feedback
                    (UserClickedRadioButtonInMeaning trial.option1)
                , View.radio
                    trial.option2
                    (state.userAnswer == trial.option2)
                    (isCorrect trial.option2)
                    feedback
                    (UserClickedRadioButtonInMeaning trial.option2)
                , View.radio
                    trial.option3
                    (state.userAnswer == trial.option3)
                    (isCorrect trial.option3)
                    feedback
                    (UserClickedRadioButtonInMeaning trial.option3)
                ]
                UserClickedFeedbackButtonInMeaning
                UserClickedNextTrialButtonInMeaning
            ]

        _ ->
            [ text "impossible case you should change the data model" ]



-- UPDATE


type Msg
    = BrowserChangedUrl Url
    | UserClickedLink Browser.UrlRequest
    | UserClickedStartExperimentButton
    | ServerRespondedWithMeaningInput (Result Http.Error (List Meaning.Trial))
    | ServerRespondedWithTranslationTrials (Result Http.Error (List Translation.Trial))
    | UserClickedRadioButtonInMeaning String
    | UserClickedFeedbackButtonInMeaning
    | UserClickedNextTrialButtonInMeaning


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    let
        currentMeaningState =
            E.getState model.experiment |> Maybe.withDefault Meaning.initState
    in
    case msg of
        BrowserChangedUrl url ->
            ( { model | route = Route.fromUrl url }
            , fetchData model.route
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
            , Cmd.batch
                [ Nav.pushUrl model.key (Url.Builder.absolute [ "meaning" ] [])
                , Meaning.getTrialsFromServer ServerRespondedWithMeaningInput
                ]
            )

        ServerRespondedWithMeaningInput (Result.Ok data) ->
            ( { model
                | experiment =
                    E.Ready
                        ( E.Meaning data Meaning.initState, E.MainLoop 0 False )
              }
            , Cmd.none
            )

        ServerRespondedWithTranslationTrials (Result.Err reason) ->
            ( { model | experiment = E.Failure reason }, Cmd.none )

        ServerRespondedWithTranslationTrials (Result.Ok data) ->
            ( { model
                | translation =
                    E.Ready
                        ( E.Meaning data Translation.initState, E.MainLoop 0 False )
              }
            , Cmd.none
            )

        ServerRespondedWithMeaningInput (Result.Err reason) ->
            ( { model | experiment = E.Failure reason }, Cmd.none )

        UserClickedRadioButtonInMeaning newChoice ->
            ( { model
                | experiment =
                    model.experiment
                        |> Meaning.updateState { currentMeaningState | userAnswer = newChoice }
              }
            , Cmd.none
            )

        UserClickedFeedbackButtonInMeaning ->
            ( { model
                | experiment =
                    model.experiment
                        |> E.toggleFeedback
              }
            , Cmd.none
            )

        UserClickedNextTrialButtonInMeaning ->
            ( { model
                | experiment =
                    model.experiment
                        |> E.toggleFeedback
                        |> Meaning.updateState { currentMeaningState | userAnswer = "" }
                        |> E.nextTrial
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
