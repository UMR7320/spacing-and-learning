module Main exposing (main)

import Array
import Browser
import Browser.Events exposing (onKeyDown, onKeyPress)
import Browser.Navigation as Nav
import Canvas
import Data
import Dict
import DnDList
import DnDList.Groups exposing (Model)
import Experiment.Acceptability as Acceptability
import Experiment.CloudWords as CloudWords
import Experiment.Experiment as E
import Experiment.Meaning as Meaning exposing (..)
import Experiment.Scrabble as Scrabble
import Experiment.Synonym as Synonym
import Experiment.Translation as Translation
import Html.Attributes
import Html.Styled exposing (..)
import Html.Styled.Attributes exposing (attribute, class, disabled, href, type_)
import Html.Styled.Events exposing (onClick)
import Html.Styled.Keyed as Keyed
import Http
import Json.Decode exposing (errorToString)
import List.Extra
import Random
import Random.Extra
import Random.List
import RemoteData exposing (RemoteData, WebData)
import Result exposing (Result)
import Route exposing (Route(..))
import Set exposing (Set)
import Task
import Time
import Url exposing (Url)
import Url.Builder
import View exposing (navIn, navOut)


type alias Flags =
    {}



--DATA


toKeyedItem : List String -> List ( String, String )
toKeyedItem =
    List.map (\v -> ( "key-" ++ v, v ))



-- SYSTEM


config : DnDList.Config E.KeyedItem
config =
    { beforeUpdate = \_ _ list -> list
    , movement = DnDList.Free
    , listen = DnDList.OnDrag
    , operation = DnDList.Swap
    }


system : DnDList.System E.KeyedItem Msg
system =
    DnDList.create config UserDragsLetter



-- MODEL


type alias Model =
    { key : Nav.Key
    , route : Route.Route
    , meaningTask : E.Experiment
    , translationTask : E.Experiment
    , synonymTask : E.Experiment
    , scrabbleTask : E.Experiment
    , acceptabilityTask : Acceptability.Task
    , cloudWords : Dict.Dict String Bool
    , dnd : DnDList.Model

    --, updateTime : Time.Posix
    --, scrabbleItems : List E.KeyedItem
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
      , synonymTask = E.NotStarted
      , scrabbleTask = E.NotStarted
      , acceptabilityTask = Acceptability.NotStarted
      , cloudWords = Dict.fromList CloudWords.words
      , dnd = system.model
      }
    , fetchData route
    )


view : Model -> Browser.Document Msg
view model =
    { title = project.title
    , body = [ body model |> div [] |> toUnstyled ]
    }


viewCloud : Model -> Html Msg
viewCloud model =
    div [ class "grid grid-flow-col grid-rows-4 auto-cols-max gap-4 " ]
        (List.map
            (\word ->
                let
                    value =
                        Dict.get word model.cloudWords
                in
                label [ class "border-2 p-2 text-black align-baseline flex flex-row" ]
                    [ input
                        [ type_ "checkbox"
                        , Html.Styled.Attributes.checked <| Maybe.withDefault False value
                        , Html.Styled.Events.onClick <|
                            UserToggledInCloudWords word
                        ]
                        []
                    , span [ class "pl-4" ] [ text word ]
                    ]
            )
            (Dict.keys model.cloudWords)
        )


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
                , div [ class "flex flex-col" ]
                    [ startButton
                    , startTranslation
                    , startSynonym
                    , startScrabble
                    , startCloudWords
                    , View.navIn "Go to Acceptability >" "/acceptability"
                    ]

                --, div [ class "grid grid-flow-col grid-rows-4  gap-4" ] words
                --, viewCloud model
                ]

            Meaning ->
                viewExperiment model

            Translation ->
                viewTranslationTask model

            Synonym ->
                viewSynonymTask model

            Scrabble ->
                viewScrabbleTask model

            CloudWords ->
                [ viewCloud model ]

            Acceptability ->
                [ Acceptability.view model.acceptabilityTask { nextTrialMsg = UserClickedNextTrialInAcceptability } ]

            Home ->
                [ text "home ?" ]

            NotFound ->
                View.notFound
    ]



--words : List (Html msg)


viewScrabbleTask model =
    case model.scrabbleTask of
        E.NotStarted ->
            [ text "NotAsked" ]

        E.Loading ->
            [ text "Loading" ]

        E.DoingScrabble (E.MainLoop trials state ntrial feedback) ->
            let
                currentTrial =
                    Array.get ntrial (Array.fromList trials) |> Maybe.withDefault Scrabble.defaultTrial
            in
            [ h3 [] [ text "Listen to the sound and write what you hear" ]
            , View.simpleAudioPlayer currentTrial.audioWord.url
            , state.scrambledLetter
                |> List.indexedMap (itemView model.dnd)
                |> Keyed.node "div" containerStyles
            , ghostView model.dnd state.scrambledLetter
            , View.button
                { message = UserClickedNextTrialButtonInScrabble
                , txt = "Next Trial "
                , isDisabled = False
                }
            ]

        E.DoingScrabble E.End ->
            [ h3 [] [ text "C'est la fin, merci de votre participation ! 🎉️" ] ]

        _ ->
            [ text "unexpected view in viewscrabble. Please update Main.viewScrabbleTask or report this error message" ]


itemView : DnDList.Model -> Int -> E.KeyedItem -> ( String, Html Msg )
itemView dnd index ( key, item ) =
    let
        itemId : String
        itemId =
            "id-" ++ item
    in
    case system.info dnd of
        Just { dragIndex } ->
            if dragIndex /= index then
                ( key
                , div
                    (Html.Styled.Attributes.id itemId
                        :: itemStyles green
                        ++ List.map Html.Styled.Attributes.fromUnstyled (system.dropEvents index itemId)
                    )
                    [ text item ]
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
                    :: itemStyles green
                    ++ List.map Html.Styled.Attributes.fromUnstyled (system.dragEvents index itemId)
                )
                [ text item ]
            )


ghostView : DnDList.Model -> List E.KeyedItem -> Html Msg
ghostView dnd items =
    let
        maybeDragItem : Maybe E.KeyedItem
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
            div (itemStyles ghostGreen ++ List.map Html.Styled.Attributes.fromUnstyled (system.ghostStyles dnd)) [ text item ]

        Nothing ->
            text ""


startButton : Html Msg
startButton =
    View.navIn "Go to Meaning >" "/meaning"


startTranslation : Html Msg
startTranslation =
    View.navIn "Go to Translation >" "/translation"


startSynonym : Html Msg
startSynonym =
    View.navIn "Go to Synonym >" "/synonym"


startScrabble : Html Msg
startScrabble =
    View.navIn "Go to Scrabble >" "/scrabble"


startCloudWords : Html Msg
startCloudWords =
    View.navIn "Go to CloudWords >" "/cloudwords"


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
            [ h1 [] [ text "NotStarted" ]
            ]

        E.Loading ->
            [ h1 [] [ text "loading" ]
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
            [ h1 [] [ text "NotStarted" ]
            , p
                [ class "max-w-xl text-xl mb-8" ]
                [ text "Une expérience visant à mieux comprendre l'acquisition de nouvelles structures grammaticales en langue anglaise. "
                ]
            , div [] [ startButton ]
            ]

        E.Loading ->
            [ h1 [] [ text "loading" ]
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
            [ h1 [] [ text "Failure" ]
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


viewSynonymTask : Model -> List (Html Msg)
viewSynonymTask model =
    case model.synonymTask of
        E.NotStarted ->
            [ h1 [] [ text "NotStarted" ]
            ]

        E.Loading ->
            [ h1 [] [ text "loading" ]
            ]

        E.Failure reason ->
            [ h1 [] [ text ("Failure" ++ buildErrorMessage reason) ]
            ]

        E.DoingSynonym (E.MainLoop trials state trialn feedback) ->
            let
                trial =
                    Array.get
                        trialn
                        (Array.fromList trials)
                        |> Maybe.withDefault Synonym.defaultTrial

                isCorrect optionN =
                    optionN == trial.target
            in
            [ Meaning.view
                model.synonymTask
                [ View.radio
                    trial.distractor1
                    (state.userAnswer == trial.distractor1)
                    (isCorrect trial.distractor1)
                    feedback
                    (UserClickedRadioButtonInSynonym trial.distractor1)
                , View.radio
                    trial.distractor2
                    (state.userAnswer == trial.distractor2)
                    (isCorrect trial.distractor2)
                    feedback
                    (UserClickedRadioButtonInSynonym trial.distractor2)
                , View.radio
                    trial.distractor3
                    (state.userAnswer == trial.distractor3)
                    (isCorrect trial.distractor3)
                    feedback
                    (UserClickedRadioButtonInSynonym trial.distractor3)
                , View.radio
                    trial.target
                    (state.userAnswer == trial.target)
                    (isCorrect trial.target)
                    feedback
                    (UserClickedRadioButtonInSynonym trial.target)
                ]
                UserClickedFeedbackButtonInSynonym
                UserClickedNextTrialButtonInSynonym
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


type Msg
    = BrowserChangedUrl Url
    | ServerRespondedWithMeaningInput (Result Http.Error (List E.TrialMeaning))
    | ServerRespondedWithTranslationTrials (Result Http.Error (List E.TranslationInput))
    | ServerRespondedWithScrabbleTrials (Result Http.Error (List E.ScrabbleTrial))
    | ServerRespondedWithSynonymTrials (Result Http.Error (List E.SynonymTrial))
    | ServerRespondedWithAcceptabilityTrials (Result Http.Error (List Acceptability.Trial))
    | Shuffled Msg
    | UserClickedLink Browser.UrlRequest
    | UserClickedRadioButtonInMeaning String
    | UserClickedRadioButtonInTranslation String
    | UserClickedRadioButtonInSynonym String
    | UserClickedFeedbackButtonInMeaning
    | UserClickedFeedbackButtonInTranslation
    | UserClickedFeedbackButtonInSynonym
    | UserClickedNextTrialButtonInMeaning
    | UserClickedNextTrialButtonInTranslation
    | UserClickedNextTrialButtonInSynonym
    | UserClickedNextTrialButtonInScrabble
    | UserClickedNextTrialInAcceptability
    | UserToggledInCloudWords String
    | UserDragsLetter DnDList.Msg
    | UserPressedKey Acceptability.Evaluation
    | WithTime Msg Time.Posix



--| GetTimeAndThen (Time.Posix -> Msg)


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

        currentSynonymState =
            case E.getState model.synonymTask of
                E.SynonymStateType x ->
                    x

                _ ->
                    Synonym.initState

        currentScrabbleState =
            case E.getState model.scrabbleTask of
                E.ScrabbleStateType x ->
                    x

                _ ->
                    Scrabble.initState

        ( currentScrabbleTrialn, nextScrabbleTrial ) =
            case model.scrabbleTask of
                E.DoingScrabble (E.MainLoop trials state trialn feedback) ->
                    ( Array.get trialn (Array.fromList trials) |> Maybe.withDefault Scrabble.defaultTrial
                    , Array.get (trialn + 1) (Array.fromList trials) |> Maybe.withDefault Scrabble.defaultTrial
                    )

                _ ->
                    ( Scrabble.defaultTrial, Scrabble.defaultTrial )
    in
    case msg of
        BrowserChangedUrl url ->
            case Route.fromUrl url of
                Meaning ->
                    ( { model | route = Route.fromUrl url, meaningTask = E.Loading }
                    , fetchData (Route.fromUrl url)
                    )

                Translation ->
                    ( { model | route = Route.fromUrl url, translationTask = E.Loading }
                    , fetchData (Route.fromUrl url)
                    )

                Synonym ->
                    ( { model | route = Route.fromUrl url, translationTask = E.Loading }
                    , fetchData (Route.fromUrl url)
                    )

                Scrabble ->
                    ( { model | route = Route.fromUrl url, scrabbleTask = E.Loading }
                    , fetchData (Route.fromUrl url)
                    )

                ExperimentStart ->
                    ( { model | route = Route.fromUrl url }
                    , fetchData (Route.fromUrl url)
                    )

                Acceptability ->
                    ( { model | route = Route.fromUrl url }
                    , fetchData (Route.fromUrl url)
                    )

                CloudWords ->
                    ( { model | route = Route.fromUrl url }
                    , Cmd.none
                    )

                Home ->
                    ( { model | route = Route.fromUrl url }
                    , Cmd.none
                    )

                NotFound ->
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

        ServerRespondedWithMeaningInput (Result.Ok data) ->
            ( model
            , Random.generate (\shuffledData -> Shuffled (ServerRespondedWithMeaningInput (Result.Ok shuffledData))) (Random.List.shuffle data)
            )

        Shuffled (ServerRespondedWithMeaningInput (Result.Ok data)) ->
            ( { model
                | meaningTask =
                    E.DoingMeaning (E.MainLoop data Meaning.initState 0 False)
              }
            , Cmd.none
            )

        ServerRespondedWithTranslationTrials (Result.Err reason) ->
            ( { model | translationTask = E.Failure reason }, Cmd.none )

        ServerRespondedWithTranslationTrials (Result.Ok data) ->
            ( model
            , Random.generate (\shuffledData -> Shuffled (ServerRespondedWithTranslationTrials (Result.Ok shuffledData))) (Random.List.shuffle data)
            )

        Shuffled (ServerRespondedWithTranslationTrials (Result.Ok data)) ->
            ( { model | translationTask = E.DoingTranslation (E.MainLoop data E.initTranslationState 0 False) }, Cmd.none )

        ServerRespondedWithSynonymTrials (Result.Err reason) ->
            ( { model | synonymTask = E.Failure reason }, Cmd.none )

        ServerRespondedWithSynonymTrials (Result.Ok data) ->
            ( model
            , Random.generate (\shuffledData -> Shuffled (ServerRespondedWithSynonymTrials (Result.Ok shuffledData))) (Random.List.shuffle data)
            )

        Shuffled (ServerRespondedWithSynonymTrials (Result.Ok data)) ->
            ( { model | synonymTask = E.DoingSynonym (E.MainLoop data E.initTranslationState 0 False) }, Cmd.none )

        ServerRespondedWithAcceptabilityTrials (Result.Ok trials) ->
            ( model
            , Random.generate (\shuffledData -> Shuffled (ServerRespondedWithAcceptabilityTrials (Result.Ok shuffledData))) (Random.List.shuffle trials)
            )

        Shuffled (ServerRespondedWithAcceptabilityTrials (Result.Ok trials)) ->
            ( { model | acceptabilityTask = Acceptability.DoingTask trials Acceptability.initState 0 [] }, Cmd.none )

        ServerRespondedWithAcceptabilityTrials (Result.Err reason) ->
            ( { model | acceptabilityTask = Acceptability.Failure reason }
            , Cmd.none
            )

        ServerRespondedWithScrabbleTrials (Result.Err reason) ->
            ( { model | scrabbleTask = E.Failure reason }, Cmd.none )

        ServerRespondedWithScrabbleTrials (Result.Ok data) ->
            let
                record =
                    Scrabble.initState

                firstTrialWord =
                    Array.get 0 (Array.fromList data)
                        |> Maybe.withDefault Scrabble.defaultTrial
                        |> .writtenWord

                --shuffleLetters : String
                shuffleLetters =
                    data
                        |> List.map
                            (\trial ->
                                Random.map3 E.ScrabbleTrial
                                    (Random.uniform trial.uid [])
                                    (trial.writtenWord
                                        |> String.toList
                                        |> Random.List.shuffle
                                        |> Random.map
                                            (\letters_ ->
                                                letters_
                                                    |> List.map String.fromChar
                                                    |> String.concat
                                            )
                                    )
                                    (Random.uniform trial.audioWord [])
                            )
                        |> Random.Extra.sequence
                        |> Random.andThen Random.List.shuffle
            in
            ( model
            , Random.generate
                (\shuffledData ->
                    Shuffled (ServerRespondedWithScrabbleTrials (Result.Ok shuffledData))
                )
                shuffleLetters
            )

        Shuffled (ServerRespondedWithScrabbleTrials (Result.Ok trials)) ->
            let
                record =
                    Scrabble.initState

                firstTrialWord =
                    Array.get 0 (Array.fromList trials)
                        |> Maybe.withDefault Scrabble.defaultTrial
                        |> .writtenWord
            in
            ( { model
                | scrabbleTask =
                    E.DoingScrabble
                        (E.MainLoop trials
                            { record
                                | userAnswer =
                                    firstTrialWord
                                , scrambledLetter = toItems firstTrialWord
                            }
                            0
                            False
                        )
              }
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

        UserClickedRadioButtonInSynonym newChoice ->
            ( { model
                | synonymTask =
                    model.synonymTask
                        |> E.updateState (E.SynonymStateType { currentSynonymState | userAnswer = newChoice })
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

        UserClickedFeedbackButtonInSynonym ->
            ( { model
                | synonymTask =
                    model.synonymTask
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

        UserClickedNextTrialButtonInSynonym ->
            ( { model
                | synonymTask =
                    model.synonymTask
                        |> E.toggleFeedback
                        |> E.updateState (E.SynonymStateType { currentSynonymState | userAnswer = "" })
                        |> E.nextTrial
              }
            , Cmd.none
            )

        UserClickedNextTrialInAcceptability ->
            let
                state =
                    Acceptability.getState model.acceptabilityTask
            in
            ( model
            , Task.perform (\time -> WithTime UserClickedNextTrialInAcceptability time) Time.now
            )

        UserPressedKey evaluation ->
            ( model
            , Task.perform (\time -> WithTime (UserPressedKey evaluation) time) Time.now
            )

        UserClickedNextTrialButtonInScrabble ->
            ( { model
                | scrabbleTask =
                    model.scrabbleTask
                        |> E.updateState
                            (E.ScrabbleStateType
                                { currentScrabbleState
                                    | userAnswer = nextScrabbleTrial.writtenWord
                                    , scrambledLetter = toItems nextScrabbleTrial.writtenWord
                                }
                            )
                        |> E.nextTrial
              }
            , Cmd.none
            )

        UserToggledInCloudWords word ->
            ( { model | cloudWords = CloudWords.toggle word model.cloudWords }, Cmd.none )

        UserDragsLetter dndmsg ->
            let
                items_ =
                    toItems currentScrabbleState.userAnswer

                ( dnd, items ) =
                    system.update dndmsg model.dnd currentScrabbleState.scrambledLetter
            in
            ( { model
                | dnd = dnd
                , scrabbleTask = E.updateState (E.ScrabbleStateType { currentScrabbleState | scrambledLetter = items }) model.scrabbleTask
              }
            , system.commands dnd
            )

        WithTime (UserPressedKey evaluation) time ->
            let
                prevState =
                    Acceptability.getState model.acceptabilityTask

                trial =
                    Acceptability.getCurrentTrial model.acceptabilityTask
            in
            ( { model
                | acceptabilityTask =
                    model.acceptabilityTask
                        |> Acceptability.updateState { prevState | endedAt = Just time, evaluation = evaluation, trialuid = trial.uid }
                        |> Acceptability.recordState
                        |> Acceptability.nextTrial time
              }
            , Cmd.none
            )

        _ ->
            ( model, Cmd.none )



--generateTrials : List E.ScrabbleTrial -> Random.Generator (List E.ScrabbleTrial)


generateTrials trials =
    Debug.todo ""


toItems : String -> List E.KeyedItem
toItems string =
    string
        |> String.toList
        |> List.map String.fromChar
        |> toKeyedItem


fromItems : List E.KeyedItem -> String
fromItems =
    List.map Tuple.second >> String.join ""


subscriptions : Model -> Sub Msg
subscriptions model =
    Sub.batch
        [ system.subscriptions model.dnd
        , onKeyDown keyDecoder
        ]


keyDecoder : Json.Decode.Decoder Msg
keyDecoder =
    Json.Decode.map toEvaluation (Json.Decode.field "key" Json.Decode.string)


toEvaluation : String -> Msg
toEvaluation x =
    case x of
        "y" ->
            UserPressedKey Acceptability.SentenceCorrect

        "n" ->
            UserPressedKey Acceptability.SentenceIncorrect

        _ ->
            UserPressedKey Acceptability.NoEvaluation


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


fetchData : Route -> Cmd Msg
fetchData route =
    case route of
        Meaning ->
            Meaning.getTrialsFromServer ServerRespondedWithMeaningInput

        Translation ->
            Translation.getTrialsFromServer ServerRespondedWithTranslationTrials

        Synonym ->
            Synonym.getTrialsFromServer ServerRespondedWithSynonymTrials

        Scrabble ->
            Scrabble.getTrialsFromServer ServerRespondedWithScrabbleTrials

        Acceptability ->
            Acceptability.getTrialsFromServer ServerRespondedWithAcceptabilityTrials

        CloudWords ->
            Cmd.none

        ExperimentStart ->
            Cmd.none

        Home ->
            Cmd.none

        NotFound ->
            Cmd.none


green : String
green =
    "#3da565"


ghostGreen =
    "#2f804e"


containerStyles : List (Html.Styled.Attribute msg)
containerStyles =
    [ Html.Styled.Attributes.style "display" "flex"
    , Html.Styled.Attributes.style "flex-wrap" "wrap"
    , Html.Styled.Attributes.style "align-items" "center"

    --, Html.Styled.Attributes.style "justify-content" "center"
    , Html.Styled.Attributes.style "padding-top" "2em"
    ]


itemStyles : String -> List (Html.Styled.Attribute msg)
itemStyles color =
    [ Html.Styled.Attributes.style "width" "5rem"
    , Html.Styled.Attributes.style "height" "5rem"
    , Html.Styled.Attributes.style "background-color" color
    , Html.Styled.Attributes.style "border-radius" "8px"
    , Html.Styled.Attributes.style "color" "white"
    , Html.Styled.Attributes.style "cursor" "pointer"
    , Html.Styled.Attributes.style "margin" "0 2em 2em 0"
    , Html.Styled.Attributes.style "display" "flex"
    , Html.Styled.Attributes.style "align-items" "center"
    , Html.Styled.Attributes.style "justify-content" "center"
    ]


project : { description : String, title : String, url : String }
project =
    { title = "Apprentissage et espacement"
    , description = """
        Une expérience visant à mieux comprendre l'acquisition de nouvelles structures grammaticales en langue anglaise. 
      """
    , url = Url.Builder.absolute [ "start" ] []
    }
