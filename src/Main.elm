port module Main exposing (main)

import Array
import Browser
import Browser.Events exposing (onKeyDown, onKeyPress)
import Browser.Navigation as Nav
import Data
import Dict
import DnDList
import DnDList.Groups exposing (Model)
import Experiment.Acceptability as Acceptability exposing (nextTrial)
import Experiment.CU1 as CU1
import Experiment.CU2 as CU2
import Experiment.CloudWords as CloudWords
import Experiment.Experiment as E
import Experiment.Meaning as Meaning exposing (..)
import Experiment.Scrabble as Scrabble
import Experiment.SpellingLvl1 as SpellingLvl1
import Experiment.Synonym as Synonym
import Experiment.Translation as Translation
import ExperimentInfo exposing (Session(..))
import Html.Styled exposing (..)
import Html.Styled.Attributes exposing (attribute, class, disabled, href, type_)
import Html.Styled.Events exposing (onClick)
import Html.Styled.Keyed as Keyed
import Http
import Icons
import Json.Decode exposing (errorToString)
import Json.Encode as Encode
import PsychTask
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
import User
import View exposing (navIn, navOut)


type alias Flags =
    {}


port playAudio : String -> Cmd msg



--DATA


toKeyedItem : List String -> List ( String, String )
toKeyedItem letters =
    List.map (\( lett, rec ) -> ( "key-" ++ lett ++ String.fromInt rec, lett )) (Scrabble.dedupe letters)



-- SYSTEM


config : DnDList.Config E.KeyedItem
config =
    { beforeUpdate = \_ _ list -> list
    , movement = DnDList.Free
    , listen = DnDList.OnDrag
    , operation =
        DnDList.Swap
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
    , scrabbleTask : PsychTask.Task Scrabble.Trial Scrabble.State
    , spellingLvl1 : PsychTask.Task SpellingLvl1.Trial SpellingLvl1.State
    , cuLvl2 : PsychTask.Task CU2.Trial CU2.State
    , cu1 : PsychTask.Task CU1.Trial CU1.State
    , acceptabilityTask : Acceptability.Task
    , cloudWords : Dict.Dict String Bool
    , dnd : DnDList.Model
    , infos : RemoteData Http.Error (List ExperimentInfo.Task)
    , user : Maybe User.User
    , userFirstName : String
    , userEmail : String
    , optionsOrder : List Int
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
      , scrabbleTask = PsychTask.NotStartedYet
      , spellingLvl1 = PsychTask.NotStartedYet
      , cuLvl2 = PsychTask.NotStartedYet
      , cu1 = PsychTask.NotStartedYet
      , acceptabilityTask = Acceptability.NotStarted
      , cloudWords = Dict.fromList CloudWords.words
      , dnd = system.model
      , infos = RemoteData.Loading
      , user = Nothing
      , userFirstName = ""
      , userEmail = ""
      , optionsOrder = [ 0, 1, 2, 3 ]
      }
    , Cmd.batch [ fetchData route, ExperimentInfo.getInfos ServerRespondedWithInfos ]
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
        , navIn "Log in" "/login"
        , navOut "BCL" "https://bcl.cnrs.fr/"
        , navOut "L'équipe" "https://bcl.cnrs.fr/rubrique225"
        ]
    , View.container <|
        case model.infos of
            RemoteData.Success infos ->
                let
                    infosAsDict =
                        infos
                            |> List.map
                                (\info ->
                                    ( info.name
                                    , { uid = info.uid
                                      , session = info.session
                                      , type_ = info.type_
                                      , name = info.name
                                      , url = info.url
                                      , description = info.description
                                      , instructions = info.instructions
                                      , instructions_short = info.instructions_short
                                      , end = info.end
                                      }
                                    )
                                )
                            |> Dict.fromList
                in
                case ( model.route, model.user ) of
                    ( ExperimentStart, Just user ) ->
                        let
                            toCard =
                                \info ->
                                    div [ class "my-1 px-1 w-full md:w-1/2 lg:my-4 lg:px-4 lg:w-13" ]
                                        [ Html.Styled.article [ class "overflow-hidden rounded-lg shadow-lg" ]
                                            [ Html.Styled.img [ class "block h-auto w-full", Html.Styled.Attributes.src "https://picsum.photos/600/400/?random" ] []
                                            , Html.Styled.header [ class "flex items-center justify-between leading-tight p-2 md:p4" ]
                                                [ h1 [ class "text-lg" ] [ a [ Html.Styled.Attributes.href info.url ] [ text info.name ] ]
                                                ]
                                            , p [ class "px-2 overflow-ellipsis" ] [ text info.description ]
                                            , Html.Styled.footer
                                                [ class "flex items-center justify-between leading-none p-2 md:p-4" ]
                                                [ div [ class "bg-green-500 rounded-lg p-2 text-white" ] [ text (ExperimentInfo.typeToString info.type_) ]
                                                , div [ class "bg-blue-500 rounded-lg p-2 text-white" ] [ text (ExperimentInfo.sessionToString info.session) ]
                                                ]
                                            ]
                                        ]

                            viewCard with =
                                infos
                                    |> List.filter with
                                    |> List.map toCard
                                    |> div [ class "flex flex-wrap -mx-1 px-4 md:px-12" ]
                        in
                        [ h1 [] [ text "Apprentissage et Espacement" ]
                        , p
                            [ class "max-w-xl text-xl mb-8" ]
                            [ text "Une expérience visant à mieux comprendre l'acquisition de nouvelles structures grammaticales en langue anglaise. "
                            ]
                        , h2 [] [ text "Session 1" ]
                        , viewCard (\info -> info.session == Session1)
                        , h2 [] [ text "Session 2" ]
                        , viewCard (\info -> info.session == Session2)
                        , h2 [] [ text "Session 3" ]
                        , viewCard (\info -> info.session == Session3)
                        , h2 [] [ text "Pretests" ]
                        , viewCard (\info -> info.session == Pretest)
                        ]

                    ( ExperimentStart, Nothing ) ->
                        [ text "Welcome maybe you should login ?", a [ href "/login" ] [ text "Log here" ] ]

                    ( Meaning, _ ) ->
                        viewExperiment model

                    ( Translation, _ ) ->
                        viewTranslationTask model

                    ( Synonym, _ ) ->
                        let
                            synonymInfos =
                                Dict.get "Synonym" infosAsDict
                        in
                        Synonym.viewTask model.synonymTask
                            synonymInfos
                            { toggleFeedbackMsg = UserClickedFeedbackButton model.route
                            , inputValidationMsg = UserValidatedInputInSynonym
                            , nextTrialMsg = UserClickedNextTrial model.route
                            , toMainloopMsg = UserClickedStartSynonym
                            , updateInputMsg = UserChangedInputInSynonym
                            }

                    ( Scrabble, _ ) ->
                        viewScrabbleTask model

                    ( CULevel2, _ ) ->
                        let
                            cu2Infos =
                                Dict.get "Context Understanding level 2" infosAsDict
                        in
                        [ CU2.view model.cuLvl2
                            cu2Infos
                            model.optionsOrder
                            { userClickedNextTrial = CU2 CU2.UserClickedNextTrial
                            , userClickedAudio = PlaysoundInJS
                            , radioMsg = \id -> CU2 (CU2.UserClickedRadioButton id)
                            , toggleFeedback = CU2 CU2.UserClickedToggleFeedback
                            , nextTrialMsg = CU2 CU2.UserClickedNextTrial
                            , startMainMsg = \trials -> CU2 (CU2.UserClickedStartMain trials)
                            }
                        ]

                    ( Route.CU1, _ ) ->
                        let
                            cu1Infos =
                                Dict.get "Context Understanding level 1" infosAsDict
                        in
                        [ CU1.view
                            { task = model.cu1
                            , infos = cu1Infos
                            , optionsOrder = Just model.optionsOrder
                            , nextTrialMsg = CU1 CU1.UserClickedNextTrial
                            , userClickedAudio = Nothing
                            , radioMsg = Just (\id -> CU1 (CU1.UserClickedRadioButton id))
                            , toggleFeedbackMsg = CU1 CU1.UserClickedToggleFeedback
                            , startMainMsg = \trials -> CU1 (CU1.UserClickedStartMain trials)
                            , inputChangedMsg = Nothing
                            }
                        ]

                    ( CloudWords, _ ) ->
                        [ viewCloud model ]

                    ( Acceptability, _ ) ->
                        [ Acceptability.view model.acceptabilityTask { nextTrialMsg = UserClickedNextTrial model.route } ]

                    ( Home, _ ) ->
                        [ text "home ?" ]

                    ( LogIn, Nothing ) ->
                        let
                            fieldStyle :
                                { fieldType : String
                                , placeholder : String
                                , id : String
                                , name : String
                                , value : String
                                , msg : String -> msg
                                }
                                -> List (Attribute msg)
                            fieldStyle { fieldType, placeholder, id, name, value, msg } =
                                [ Html.Styled.Attributes.placeholder placeholder
                                , type_ fieldType
                                , Html.Styled.Attributes.required True
                                , Html.Styled.Attributes.name name
                                , Html.Styled.Attributes.id id
                                , Html.Styled.Attributes.value value
                                , Html.Styled.Events.onInput msg
                                ]

                            firstNameField =
                                input
                                    (fieldStyle
                                        { fieldType = "text"
                                        , placeholder = "John"
                                        , name = "firstName"
                                        , id = "firstName"
                                        , value = model.userFirstName
                                        , msg = UserUpdatedFirstNameFieldInLogIn
                                        }
                                    )
                                    [ label [] [ text "Your first name" ] ]

                            emailField =
                                input
                                    (fieldStyle
                                        { fieldType = "email"
                                        , placeholder = "john.doe@anydomain.com"
                                        , name = "email"
                                        , id = "email"
                                        , value = model.userEmail
                                        , msg = UserUpdatedEmailFieldInLogIn
                                        }
                                    )
                                    [ label [] [ text "What's your email address ?" ] ]
                        in
                        [ text "C'est ici qu'on se connecte ! "
                        , div
                            [ class "flex flex-col" ]
                            [ firstNameField
                            , emailField
                            , button [ Html.Styled.Events.onClick UserClickedCreateUserInLogin ] [ label [] [ text "Create user" ] ]
                            ]
                        ]

                    ( LogIn, Just (User.Authenticated user) ) ->
                        [ text <| "Hey " ++ user.firstName ++ " we already know you. You can browse the experiments now!" ]

                    ( LogIn, Just (User.Ano user) ) ->
                        [ text <| "Hey " ++ user.firstName ++ " you're anonymous. It's an impossible case. Please tell the developer !" ]

                    ( NotFound, _ ) ->
                        View.notFound

                    ( SpellingLevel1, _ ) ->
                        [ SpellingLvl1.view model.spellingLvl1
                            model.optionsOrder
                            { toggleFeedbackMsg = UserClickedFeedbackButton model.route
                            , nextTrialMsg = UserClickedNextTrial model.route
                            , radioMsg = UserClickedRadioButtonInSpellingLvl1
                            , startMainloopMsg = UserClickedStartMainloopInSpellingLvl1
                            }
                        ]

            RemoteData.NotAsked ->
                [ text "" ]

            RemoteData.Loading ->
                [ text "I'm loading the tasks related infos." ]

            RemoteData.Failure reason ->
                [ text ("I couldn't fetch the tasks data. Please report this issue :" ++ Data.buildErrorMessage reason) ]
    ]



--words : List (Html msg)


viewScrabbleTask : { a | scrabbleTask : PsychTask.Task Scrabble.Trial Scrabble.State, dnd : DnDList.Model, route : Route } -> List (Html Msg)
viewScrabbleTask model =
    let
        viewLetters scrambledLetters =
            scrambledLetters
                |> List.indexedMap (itemView model.dnd)
                |> Keyed.node "div" containerStyles

        audioButton url =
            div
                [ Html.Styled.Events.onClick (PlaysoundInJS url), class "col-start-2 col-span-4 h-8 w-8" ]
                [ fromUnstyled <| Icons.music ]

        feedback feedback_ { target, attempt } nextItem =
            case ( feedback_, target == attempt ) of
                ( True, True ) ->
                    div [ class " rounded-md text-center object-center bg-green-300 m-8" ]
                        [ p [ class "p-6 text-xl text-white" ]
                            [ text "The word you heard is spelled "
                            , span [ class "font-bold" ] [ text target ]
                            ]
                        , div [ class "pb-4" ]
                            [ View.button
                                { message = UserClickedNextTrialButtonInScrabble nextItem
                                , txt = "Next"
                                , isDisabled = False
                                }
                            ]
                        ]

                ( True, False ) ->
                    div [ class " rounded-md text-center object-center bg-red-300 m-8" ]
                        [ p [ class "p-6 text-xl text-white" ]
                            [ text "The word you heard is spelled "
                            , span [ class "font-bold" ] [ text target ]
                            ]
                        , div [ class "pb-4" ]
                            [ View.button
                                { message = UserClickedNextTrialButtonInScrabble nextItem
                                , txt = "Next"
                                , isDisabled = False
                                }
                            ]
                        ]

                _ ->
                    div []
                        [ View.button
                            { message = UserClickedFeedbackButton model.route
                            , txt = "Next item "
                            , isDisabled = False
                            }
                        ]
    in
    case model.scrabbleTask of
        PsychTask.NotStartedYet ->
            [ text "NotAsked" ]

        PsychTask.Main data ->
            case data.current of
                Just currentTrial ->
                    [ audioButton currentTrial.audioWord.url
                    , if not data.feedback then
                        div []
                            [ div [ class "col-start-2 col-span-4" ] [ viewLetters data.state.scrambledLetter ]
                            , ghostView model.dnd
                                data.state.scrambledLetter
                            ]

                      else
                        div [] []
                    , feedback
                        data.feedback
                        { target = currentTrial.target, attempt = data.state.userAnswer }
                        data.next
                    ]

                Nothing ->
                    [ text " Main is over" ]

        PsychTask.Intr data ->
            case data.current of
                Just currentTrial ->
                    [ View.viewTraining "Are you ready to test your spelling?\nClick to hear the word. Then move the letters to spell it correctly. "
                        [ audioButton currentTrial.audioWord.url
                        , div [ class "col-start-2 col-span-4" ] [ viewLetters data.state.scrambledLetter ]
                        , ghostView model.dnd data.state.scrambledLetter
                        , div [ class "col-start-2 col-span-4 pb-4" ]
                            [ feedback
                                data.feedback
                                { target = currentTrial.target, attempt = data.state.userAnswer }
                                data.next
                            ]
                        ]
                    ]

                Nothing ->
                    [ text "Intro is over"
                    , View.button
                        { message = UserClickedStartMainloopInScrabble data.mainTrials
                        , txt = "Commencer l'expérience "
                        , isDisabled = False
                        }
                    ]

        PsychTask.Over ->
            [ text "cette tâche est finie merci beaucoup" ]

        PsychTask.IntroOver ->
            [ text "L'entrainement est fini" ]


itemView : DnDList.Model -> Int -> E.KeyedItem -> ( String, Html Msg )
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
                        :: itemStyles green
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
                    :: itemStyles green
                    ++ List.map Html.Styled.Attributes.fromUnstyled (system.dragEvents index itemId)
                )
                [ text (String.toUpper item) ]
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
            div (itemStyles ghostGreen ++ List.map Html.Styled.Attributes.fromUnstyled (system.ghostStyles dnd)) [ text (String.toUpper item) ]

        Nothing ->
            text ""


startButton : Html Msg
startButton =
    View.navIn "Go to Meaning >" "/meaning"


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

                option id =
                    View.radio
                        id
                        (state.userAnswer == id)
                        (isCorrect id)
                        feedback
                        (UserClickedRadioButton model.route id)

                options =
                    [ option trial.option1
                    , option trial.option2
                    , option trial.option3
                    , option trial.definition
                    ]
                        |> List.map2 Tuple.pair model.optionsOrder
                        |> List.sortBy Tuple.first
                        |> List.map Tuple.second
            in
            [ Meaning.view model.meaningTask options (UserClickedFeedbackButton model.route) (UserClickedNextTrial model.route)
            ]

        E.DoingMeaning (E.End txt) ->
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

                option id =
                    View.radio
                        id
                        (state.userAnswer == id)
                        (isCorrect id)
                        feedback
                        (UserClickedRadioButton model.route id)
            in
            [ Meaning.view
                model.translationTask
                [ option trial.distractor1
                , option trial.distractor2
                , option trial.distractor3
                , option trial.translation1
                ]
                (UserClickedFeedbackButton model.route)
                (UserClickedNextTrial model.route)
            ]

        _ ->
            [ text "impossible case you should change the data model" ]



-- UPDATE


type Msg
    = BrowserChangedUrl Url
    | ServerRespondedWithMeaningInput (Result Http.Error (List E.TrialMeaning))
    | ServerRespondedWithTranslationTrials (Result Http.Error (List E.TranslationInput))
    | ServerRespondedWithScrabbleTrials (Result Http.Error (List Scrabble.Trial))
    | ServerRespondedWithSynonymTrials (Result Http.Error (List E.SynonymTrial))
    | ServerRespondedWithAcceptabilityTrials (Result Http.Error (List Acceptability.Trial))
    | ServerRespondedWithInfos (Result Http.Error (List ExperimentInfo.Task))
    | ServerRespondedWithUserInfo (Result Http.Error User.AuthenticatedInfo)
    | ServerRespondedWithSpellingTrials (Result Http.Error (List SpellingLvl1.Trial))
    | Shuffled Msg
    | UserClickedLink Browser.UrlRequest
    | UserClickedStartSynonym (List E.SynonymTrial)
    | UserClickedStartMainloopInSpellingLvl1 (List SpellingLvl1.Trial)
    | UserClickedStartMainloopInScrabble (List Scrabble.Trial)
    | UserClickedNextTrialButtonInScrabble (Maybe Scrabble.Trial)
    | UserClickedCreateUserInLogin
    | UserChangedInputInSynonym String
    | UserUpdatedFirstNameFieldInLogIn String
    | UserUpdatedEmailFieldInLogIn String
    | UserValidatedInputInSynonym
    | UserToggledInCloudWords String
    | UserDragsLetter DnDList.Msg
    | UserPressedKey Acceptability.Evaluation
    | WithTime Msg Time.Posix
    | RuntimeShuffledOptionsOrder (List Int)
    | UserClickedRadioButtonInSpellingLvl1 String
    | PlaysoundInJS String
    | UserClickedFeedbackButton Route
    | UserClickedNextTrial Route
    | UserClickedRadioButton Route String
    | CU1 CU1.CU1Msg
    | CU2 CU2.CU2Msg



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
            case PsychTask.getState model.scrabbleTask of
                Just x ->
                    x

                _ ->
                    Scrabble.initState

        currentSpellingState =
            PsychTask.getState model.spellingLvl1
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
                    ( { model | route = Route.fromUrl url, scrabbleTask = PsychTask.NotStartedYet }
                    , fetchData (Route.fromUrl url)
                    )

                CULevel2 ->
                    ( { model | route = Route.fromUrl url, cuLvl2 = PsychTask.NotStartedYet }, fetchData (Route.fromUrl url) )

                ExperimentStart ->
                    ( { model | route = Route.fromUrl url }
                    , fetchData (Route.fromUrl url)
                    )

                Acceptability ->
                    ( { model | route = Route.fromUrl url }
                    , fetchData (Route.fromUrl url)
                    )

                SpellingLevel1 ->
                    ( { model | route = Route.fromUrl url }
                    , fetchData (Route.fromUrl url)
                    )

                _ ->
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

        Shuffled message ->
            case message of
                ServerRespondedWithMeaningInput (Result.Ok data) ->
                    ( { model
                        | meaningTask =
                            E.DoingMeaning (E.MainLoop data Meaning.initState 0 False)
                      }
                    , Cmd.none
                    )

                ServerRespondedWithSpellingTrials (Result.Ok data) ->
                    ( { model
                        | spellingLvl1 =
                            PsychTask.startIntro
                                (List.filter (\datum -> datum.isTraining) data)
                                (List.filter (\datum -> not datum.isTraining) data)
                                SpellingLvl1.initState
                      }
                    , Cmd.none
                    )

                ServerRespondedWithTranslationTrials (Result.Ok data) ->
                    ( { model | translationTask = E.DoingTranslation (E.MainLoop data E.initTranslationState 0 False) }, Cmd.none )

                ServerRespondedWithSynonymTrials (Result.Ok data) ->
                    ( { model | synonymTask = E.DoingSynonym (E.Intro data Synonym.initState 0 False "Instructions de synonyme") }, Cmd.none )

                ServerRespondedWithAcceptabilityTrials (Result.Ok trials) ->
                    ( { model | acceptabilityTask = Acceptability.DoingTask trials Acceptability.initState 0 [] }, Cmd.none )

                ServerRespondedWithScrabbleTrials (Result.Ok trials) ->
                    let
                        record =
                            Scrabble.initState

                        trainingItems =
                            List.filter (\datum -> datum.isTraining) trials

                        mainItems =
                            List.filter (\datum -> not datum.isTraining) trials

                        firstTrialWord =
                            trainingItems
                                |> List.head
                                |> Maybe.withDefault Scrabble.defaultTrial
                                |> .writtenWord
                    in
                    ( { model
                        | scrabbleTask =
                            PsychTask.startIntro trainingItems mainItems { currentScrabbleState | userAnswer = firstTrialWord, scrambledLetter = toItems firstTrialWord }
                      }
                    , Cmd.none
                    )

                _ ->
                    ( model, Cmd.none )

        ServerRespondedWithTranslationTrials (Result.Err reason) ->
            ( { model | translationTask = E.Failure reason }, Cmd.none )

        ServerRespondedWithTranslationTrials (Result.Ok data) ->
            ( model
            , Random.generate (\shuffledData -> Shuffled (ServerRespondedWithTranslationTrials (Result.Ok shuffledData))) (Random.List.shuffle data)
            )

        ServerRespondedWithSpellingTrials (Result.Ok data) ->
            ( model
            , Random.generate (\shuffledData -> Shuffled (ServerRespondedWithSpellingTrials (Result.Ok shuffledData))) (Random.List.shuffle data)
            )

        ServerRespondedWithSynonymTrials (Result.Err reason) ->
            ( { model | synonymTask = E.Failure reason }, Cmd.none )

        ServerRespondedWithSynonymTrials (Result.Ok data) ->
            ( model
            , Random.generate (\shuffledData -> Shuffled (ServerRespondedWithSynonymTrials (Result.Ok shuffledData))) (Random.List.shuffle data)
            )

        ServerRespondedWithAcceptabilityTrials (Result.Ok trials) ->
            ( model
            , Random.generate (\shuffledData -> Shuffled (ServerRespondedWithAcceptabilityTrials (Result.Ok shuffledData))) (Random.List.shuffle trials)
            )

        ServerRespondedWithInfos infos ->
            ( { model | infos = RemoteData.fromResult infos }, Cmd.none )

        ServerRespondedWithUserInfo (Result.Ok info) ->
            ( { model | user = Just (User.Authenticated info) }, User.encoder (User.Authenticated info) |> Encode.encode 0 |> User.storeInfo )

        ServerRespondedWithUserInfo (Result.Err reason) ->
            ( model, Cmd.none )

        ServerRespondedWithAcceptabilityTrials (Result.Err reason) ->
            ( { model | acceptabilityTask = Acceptability.Failure reason }
            , Cmd.none
            )

        ServerRespondedWithScrabbleTrials (Result.Err reason) ->
            ( { model | scrabbleTask = PsychTask.Over }, Cmd.none )

        ServerRespondedWithScrabbleTrials (Result.Ok data) ->
            let
                record =
                    Scrabble.initState

                firstTrialWord =
                    Array.get 0 (Array.fromList data)
                        |> Maybe.withDefault Scrabble.defaultTrial
                        |> .writtenWord

                --shuffleLetters recreates each trial in shuffling every each word's letter. When it's done it shuffles trials.
                shuffleLetters =
                    data
                        |> List.map
                            (\trial ->
                                Random.map5 Scrabble.Trial
                                    (Random.constant trial.uid)
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
                                    (Random.constant trial.audioWord)
                                    (Random.constant trial.isTraining)
                                    (Random.constant trial.writtenWord)
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

        ServerRespondedWithMeaningInput (Result.Err reason) ->
            ( { model | meaningTask = E.Failure reason }, Cmd.none )

        UserClickedStartSynonym trials ->
            ( { model | synonymTask = E.DoingSynonym (E.MainLoop trials Synonym.initState 0 False) }, Cmd.none )

        UserClickedStartMainloopInSpellingLvl1 trials ->
            ( { model
                | spellingLvl1 =
                    PsychTask.startMain trials SpellingLvl1.initState
              }
            , Cmd.none
            )

        UserClickedCreateUserInLogin ->
            ( model, Data.sendUserData (Http.jsonBody (User.encoder (User.Ano { firstName = model.userFirstName, email = model.userEmail }))) ServerRespondedWithUserInfo User.decodeAuthenticatedInfo )

        UserPressedKey evaluation ->
            ( model
            , Task.perform (\time -> WithTime (UserPressedKey evaluation) time) Time.now
            )

        UserClickedNextTrialButtonInScrabble (Just nextTrial) ->
            ( { model
                | scrabbleTask =
                    PsychTask.next
                        { currentScrabbleState
                            | userAnswer = nextTrial.writtenWord
                            , scrambledLetter = toItems nextTrial.writtenWord
                        }
                        model.scrabbleTask
              }
            , Cmd.none
            )

        UserClickedNextTrialButtonInScrabble Nothing ->
            ( { model
                | scrabbleTask =
                    PsychTask.next currentScrabbleState model.scrabbleTask
              }
            , Cmd.none
            )

        UserChangedInputInSynonym new ->
            ( { model | synonymTask = E.updateState (E.SynonymStateType { currentSynonymState | userAnswer = new }) model.synonymTask }, Cmd.none )

        UserUpdatedFirstNameFieldInLogIn new ->
            ( { model | userFirstName = new }, Cmd.none )

        UserUpdatedEmailFieldInLogIn new ->
            ( { model | userEmail = new }, Cmd.none )

        UserValidatedInputInSynonym ->
            ( { model | synonymTask = model.synonymTask |> E.toggleFeedback }, Cmd.none )

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
                , scrabbleTask = PsychTask.update { currentScrabbleState | scrambledLetter = items, userAnswer = String.concat (List.map Tuple.second items) } model.scrabbleTask
              }
            , system.commands dnd
            )

        WithTime message time ->
            case message of
                UserPressedKey evaluation ->
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

        RuntimeShuffledOptionsOrder newOrder ->
            ( { model | optionsOrder = newOrder }, Cmd.none )

        UserClickedRadioButtonInSpellingLvl1 newChoice ->
            case currentSpellingState of
                Just prevState ->
                    ( { model
                        | spellingLvl1 =
                            PsychTask.update { prevState | userAnswer = newChoice } model.spellingLvl1
                      }
                    , Cmd.none
                    )

                Nothing ->
                    ( model, Cmd.none )

        UserClickedStartMainloopInScrabble trials ->
            case trials of
                [] ->
                    ( { model | scrabbleTask = PsychTask.Over }, Cmd.none )

                x :: _ ->
                    ( { model | scrabbleTask = PsychTask.startMain trials { currentScrabbleState | userAnswer = x.writtenWord, scrambledLetter = toItems x.writtenWord } }, Cmd.none )

        ServerRespondedWithSpellingTrials (Err _) ->
            ( model, Cmd.none )

        PlaysoundInJS url ->
            ( model, playAudio url )

        UserClickedFeedbackButton context ->
            case context of
                Route.Scrabble ->
                    ( { model | scrabbleTask = PsychTask.toggle model.scrabbleTask }, Cmd.none )

                Route.Meaning ->
                    ( { model
                        | meaningTask =
                            model.meaningTask
                                |> E.toggleFeedback
                      }
                    , Cmd.none
                    )

                Route.Synonym ->
                    ( { model
                        | synonymTask =
                            model.synonymTask
                                |> E.toggleFeedback
                      }
                    , Cmd.none
                    )

                Route.Translation ->
                    ( { model
                        | translationTask =
                            model.translationTask
                                |> E.toggleFeedback
                      }
                    , Cmd.none
                    )

                Route.SpellingLevel1 ->
                    ( { model
                        | spellingLvl1 =
                            model.spellingLvl1
                                |> PsychTask.toggle
                      }
                    , Cmd.none
                    )

                _ ->
                    ( model, Cmd.none )

        UserClickedNextTrial context ->
            case context of
                Route.Scrabble ->
                    ( { model | scrabbleTask = PsychTask.toggle model.scrabbleTask }, Cmd.none )

                Route.Meaning ->
                    ( { model
                        | meaningTask =
                            model.meaningTask
                                |> E.toggleFeedback
                                |> E.updateState (E.MeaningState { currentMeaningState | userAnswer = "" })
                                |> E.nextTrial
                      }
                    , Random.generate RuntimeShuffledOptionsOrder (Random.List.shuffle model.optionsOrder)
                    )

                Route.Synonym ->
                    ( { model
                        | synonymTask =
                            model.synonymTask
                                |> E.toggleFeedback
                                |> E.updateState (E.SynonymStateType { currentSynonymState | userAnswer = "" })
                                |> E.nextTrial
                      }
                    , Cmd.none
                    )

                Route.Translation ->
                    ( { model
                        | translationTask =
                            model.translationTask
                                |> E.toggleFeedback
                                |> E.updateState (E.TranslationState { currentTranslationState | userAnswer = "" })
                                |> E.nextTrial
                      }
                    , Cmd.none
                    )

                Route.SpellingLevel1 ->
                    ( { model
                        | spellingLvl1 =
                            PsychTask.next SpellingLvl1.initState model.spellingLvl1

                        -- ICI GROS BUG À VENIR, il faut reset uniquement la réponse du volontaire
                      }
                    , Random.generate RuntimeShuffledOptionsOrder (Random.List.shuffle model.optionsOrder)
                    )

                Route.Acceptability ->
                    let
                        state =
                            Acceptability.getState model.acceptabilityTask
                    in
                    ( model
                    , Task.perform (\time -> WithTime (UserClickedNextTrial model.route) time) Time.now
                    )

                _ ->
                    Debug.todo "Other contexts when next trial is clicked"

        UserClickedRadioButton context newChoice ->
            case context of
                Route.Meaning ->
                    ( { model
                        | meaningTask =
                            model.meaningTask
                                |> E.updateState (E.MeaningState { currentMeaningState | userAnswer = newChoice })
                      }
                    , Cmd.none
                    )

                Route.Translation ->
                    ( { model
                        | translationTask =
                            model.translationTask
                                |> E.updateState (E.TranslationState { currentMeaningState | userAnswer = newChoice })
                      }
                    , Cmd.none
                    )

                Route.Synonym ->
                    ( { model
                        | synonymTask =
                            model.synonymTask
                                |> E.updateState (E.SynonymStateType { currentSynonymState | userAnswer = newChoice })
                      }
                    , Cmd.none
                    )

                _ ->
                    Debug.todo "other contexts when radio button is clicked"

        CU2 message ->
            case message of
                CU2.UserClickedNextTrial ->
                    ( { model | cuLvl2 = PsychTask.next CU2.initState model.cuLvl2 }, Cmd.none )

                CU2.UserClickedToggleFeedback ->
                    ( { model | cuLvl2 = PsychTask.toggle model.cuLvl2 }, Cmd.none )

                CU2.UserClickedRadioButton newChoice ->
                    ( { model | cuLvl2 = PsychTask.update { uid = "", userAnswer = newChoice } model.cuLvl2 }, Cmd.none )

                CU2.ServerRespondedWith (Ok results) ->
                    let
                        trainingTrials =
                            List.filter (\datum -> datum.isTraining) results

                        mainTrials =
                            List.filter (\datum -> not datum.isTraining) results
                    in
                    ( { model | cuLvl2 = PsychTask.startIntro trainingTrials mainTrials CU2.initState }, Cmd.none )

                CU2.ServerRespondedWith (Err reason) ->
                    Debug.todo ""

                CU2.UserClickedStartIntro trials ->
                    Debug.todo ""

                CU2.UserClickedStartMain trials ->
                    ( { model | cuLvl2 = PsychTask.startMain trials CU2.initState }, Cmd.none )

        CU1 message ->
            case message of
                CU1.UserClickedNextTrial ->
                    ( { model | cu1 = PsychTask.next CU1.initState model.cu1 }, Cmd.none )

                CU1.UserClickedToggleFeedback ->
                    ( { model | cu1 = PsychTask.toggle model.cu1 }, Cmd.none )

                CU1.UserClickedRadioButton newChoice ->
                    ( { model | cu1 = PsychTask.update { uid = "", userAnswer = newChoice } model.cu1 }, Cmd.none )

                CU1.ServerRespondedWith (Ok results) ->
                    let
                        trainingTrials =
                            List.filter (\datum -> datum.isTraining) results

                        mainTrials =
                            List.filter (\datum -> not datum.isTraining) results
                    in
                    ( { model | cu1 = PsychTask.startIntro trainingTrials mainTrials CU1.initState }, Cmd.none )

                CU1.ServerRespondedWith (Err reason) ->
                    ( model, Cmd.none )

                CU1.UserClickedStartIntro trials ->
                    Debug.todo ""

                CU1.UserClickedStartMain trials ->
                    ( { model | cu1 = PsychTask.startMain trials CU2.initState }, Cmd.none )


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

        SpellingLevel1 ->
            SpellingLvl1.getTrialsFromServer ServerRespondedWithSpellingTrials

        CULevel2 ->
            CU2.getTrialsFromServer (\trials -> CU2 (CU2.ServerRespondedWith trials))

        Route.CU1 ->
            CU1.getTrialsFromServer (\trials -> CU1 (CU1.ServerRespondedWith trials))

        _ ->
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
    , class "font-bold text-lg"
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
