module Main exposing (main)

import Array
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


type alias Model =
    { key : Nav.Key
    , route : Route.Route
    , meaningTask : E.Experiment
    , translationTask : E.Experiment
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
      , meaningTask = E.NotStarted
      , translationTask = E.NotStarted
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
                , div [ class "flex flex-col" ] [ startButton, startTranslation ]
                ]

            Meaning ->
                viewExperiment model

            Translation ->
                viewTranslationTask model

            NotFound ->
                View.notFound
    ]


startButton : Html Msg
startButton =
    button
        [ class "w-64 mb-8"
        , attribute "data-action" "start-experiment"
        , onClick UserClickedStartExperimentButton
        ]
        [ text "Commencer meaning" ]


startTranslation : Html Msg
startTranslation =
    button
        [ class "w-64"
        , attribute "data-action" "start-translation"
        , onClick UserClickedStartTranslationButton
        ]
        [ text "Commencer translation" ]


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
    case model.meaningTask of
        E.NotStarted ->
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

        E.DoingMeaning (E.MainLoop trials state trialn feedback) ->
            let
                trial =
                    Array.get
                        trialn
                        (Array.fromList trials)
                        |> Maybe.withDefault E.defaultTrial

                isCorrect optionN =
                    optionN == trial.definition
            in
            [ Meaning.view
                model.meaningTask
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
                , View.radio
                    trial.option4
                    (state.userAnswer == trial.option4)
                    (isCorrect trial.option4)
                    feedback
                    (UserClickedRadioButtonInMeaning trial.option4)
                ]
                UserClickedFeedbackButtonInMeaning
                UserClickedNextTrialButtonInMeaning
            ]

        E.DoingMeaning E.End ->
            [ h1 [] [ text "Merci de votre participation !🎉" ]
            , p
                [ class "max-w-xl text-xl mb-8" ]
                [ text "Vous trouverez dans l'e-mail que vous avez reçu les liens pour la suite de l'expérience."
                ]
            ]

        _ ->
            [ text "Unexpected view. You can take it in account in Main.viewExperiment" ]


viewTranslationTask : Model -> List (Html Msg)
viewTranslationTask model =
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
    case model.translationTask of
        E.NotStarted ->
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

        E.DoingTranslation (E.MainLoop trials state trialn feedback) ->
            let
                trial =
                    Array.get
                        trialn
                        (Array.fromList trials)
                        |> Maybe.withDefault E.defaultTranslationTrial

                isCorrect optionN =
                    optionN == trial.translation1 || optionN == trial.translation2
            in
            [ Meaning.view
                model.translationTask
                [ View.radio
                    trial.distractor1
                    (state.userAnswer == trial.distractor1)
                    (isCorrect trial.distractor1)
                    feedback
                    (UserClickedRadioButtonInTranslation trial.distractor1)
                , View.radio
                    trial.distractor2
                    (state.userAnswer == trial.distractor2)
                    (isCorrect trial.distractor2)
                    feedback
                    (UserClickedRadioButtonInTranslation trial.distractor2)
                , View.radio
                    trial.distractor3
                    (state.userAnswer == trial.distractor3)
                    (isCorrect trial.distractor3)
                    feedback
                    (UserClickedRadioButtonInTranslation trial.distractor3)
                , View.radio
                    trial.distractor4
                    (state.userAnswer == trial.distractor4)
                    (isCorrect trial.distractor4)
                    feedback
                    (UserClickedRadioButtonInTranslation trial.distractor4)
                ]
                UserClickedFeedbackButtonInTranslation
                UserClickedNextTrialButtonInTranslation
            ]

        _ ->
            [ text "impossible case you should change the data model" ]



-- UPDATE


type Msg
    = BrowserChangedUrl Url
    | UserClickedLink Browser.UrlRequest
    | UserClickedStartExperimentButton
    | UserClickedStartTranslationButton
    | ServerRespondedWithMeaningInput (Result Http.Error (List E.TrialMeaning))
    | ServerRespondedWithTranslationTrials (Result Http.Error (List E.TranslationInput))
    | UserClickedRadioButtonInMeaning String
    | UserClickedRadioButtonInTranslation String
    | UserClickedFeedbackButtonInMeaning
    | UserClickedFeedbackButtonInTranslation
    | UserClickedNextTrialButtonInMeaning
    | UserClickedNextTrialButtonInTranslation


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    let
        currentMeaningState =
            case E.getState model.meaningTask of
                E.MeaningState x ->
                    x

                _ ->
                    Meaning.initState

        currentTranslationState =
            case E.getState model.translationTask of
                E.TranslationState x ->
                    x

                _ ->
                    E.initTranslationState
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
            ( { model | meaningTask = E.Loading }
            , Cmd.batch
                [ Nav.pushUrl model.key (Url.Builder.absolute [ "meaning" ] [])
                , Meaning.getTrialsFromServer ServerRespondedWithMeaningInput
                ]
            )

        UserClickedStartTranslationButton ->
            ( { model | meaningTask = E.Loading }
            , Cmd.batch
                [ Nav.pushUrl model.key (Url.Builder.absolute [ "translation" ] [])
                , Translation.getTrialsFromServer ServerRespondedWithTranslationTrials
                ]
            )

        ServerRespondedWithMeaningInput (Result.Ok data) ->
            ( { model
                | meaningTask =
                    E.DoingMeaning (E.MainLoop data Meaning.initState 0 False)
              }
            , Cmd.none
            )

        ServerRespondedWithTranslationTrials (Result.Err reason) ->
            ( { model | translationTask = E.Failure reason }, Cmd.none )

        ServerRespondedWithTranslationTrials (Result.Ok data) ->
            ( { model | translationTask = E.DoingTranslation (E.MainLoop data E.initTranslationState 0 False) }
            , Cmd.none
            )

        ServerRespondedWithMeaningInput (Result.Err reason) ->
            ( { model | meaningTask = E.Failure reason }, Cmd.none )

        UserClickedRadioButtonInMeaning newChoice ->
            ( { model
                | meaningTask =
                    model.meaningTask
                        |> E.updateState (E.MeaningState { currentMeaningState | userAnswer = newChoice })
              }
            , Cmd.none
            )

        UserClickedRadioButtonInTranslation newChoice ->
            ( { model
                | translationTask =
                    model.translationTask
                        |> E.updateState (E.TranslationState { currentMeaningState | userAnswer = newChoice })
              }
            , Cmd.none
            )

        UserClickedFeedbackButtonInMeaning ->
            ( { model
                | meaningTask =
                    model.meaningTask
                        |> E.toggleFeedback
              }
            , Cmd.none
            )

        UserClickedFeedbackButtonInTranslation ->
            ( { model
                | translationTask =
                    model.translationTask
                        |> E.toggleFeedback
              }
            , Cmd.none
            )

        UserClickedNextTrialButtonInMeaning ->
            ( { model
                | meaningTask =
                    model.meaningTask
                        |> E.toggleFeedback
                        |> E.updateState (E.MeaningState { currentMeaningState | userAnswer = "" })
                        |> E.nextTrial
              }
            , Cmd.none
            )

        UserClickedNextTrialButtonInTranslation ->
            ( { model
                | translationTask =
                    model.translationTask
                        |> E.toggleFeedback
                        |> E.updateState (E.TranslationState { currentTranslationState | userAnswer = "" })
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
