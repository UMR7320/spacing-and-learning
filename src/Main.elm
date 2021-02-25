port module Main exposing (main)

import Browser
import Browser.Events exposing (onKeyDown)
import Browser.Navigation as Nav
import Data
import Dict
import DnDList
import DnDList.Groups exposing (Model)
import Experiment.Experiment as E
import ExperimentInfo exposing (Session(..), Task)
import Html.Styled exposing (..)
import Html.Styled.Attributes exposing (class, type_)
import Html.Styled.Events
import Html.Styled.Keyed as Keyed
import Http
import Icons
import Json.Decode
import Logic
import Postest.CloudWords as CloudWords
import Postest.YN as YN
import Pretest.Acceptability as Acceptability exposing (nextTrial)
import Random
import Random.Extra
import Random.List
import RemoteData exposing (RemoteData)
import Result exposing (Result)
import Route exposing (Route(..), Session1Task(..), Session2Task(..))
import Session1.CU1 as CU1
import Session1.Meaning as Meaning
import Session1.Presentation as Presentation
import Session1.SpellingLvl1 as SpellingLvl1
import Session2.CU2 as CU2
import Session2.Scrabble as Scrabble
import Session2.Translation as Translation
import Session3.CU3 as CU3
import Session3.Spelling3 as Spelling3
import Session3.Synonym as Synonym
import Task
import Task.Parallel as Para
import Time
import Url exposing (Url)
import Url.Builder
import User
import View exposing (navIn, navOut)


type alias Flags =
    {}


port playAudio : String -> Cmd msg



-- MODEL


type Session1
    = Loading (Para.State5 Msg (List Meaning.Trial) (List SpellingLvl1.Trial) (List CU1.Trial) (List Presentation.Trial) (List ExperimentInfo.Task))
    | FailedToLoad String
    | Ready
    | NotAsked


type Session2
    = LoadingSession2 (Para.State4 Msg (List CU2.Trial) (List Scrabble.Trial) (List Translation.Trial) (List ExperimentInfo.Task))
    | FailedToLoadSession2 String
    | ReadySession2
    | NotAskedSession2


type Session3
    = LoadingSession3 (Para.State4 Msg (List CU3.Trial) (List Spelling3.Trial) (List E.SynonymTrial) (List ExperimentInfo.Task))
    | FailedToLoadSession3 String
    | ReadySession3
    | NotAskedSession3


type alias Model =
    { --                                                                88   88 888888 88 88     .dP"Y8
      --                                                                88   88   88   88 88     `Ybo."
      --                                                                Y8   8P   88   88 88  .o o.`Y8b
      --                                                                `YbodP'   88   88 88ood8 8bodP'
      key : Nav.Key
    , route : Route.Route
    , optionsOrder : List Int

    -- dnd is for drag-and-drop
    , dnd : DnDList.Model

    --                                                                  88""Yb 88""Yb 888888 888888 888888 .dP"Y8 888888
    --                                                                  88__dP 88__dP 88__     88   88__   `Ybo."   88
    --                                                                  88"""  88"Yb  88""     88   88""   o.`Y8b   88
    --                                                                  88     88  Yb 888888   88   888888 8bodP'   88
    , yn : Logic.Task YN.Trial YN.State
    , acceptabilityTask : Acceptability.Task

    --
    --                                                                  ## ###  ##  ## ###  #  ###      #
    --                                                                 #   #   #   #    #  # # # #     ##
    --                                                                  #  ##   #   #   #  # # # #      #
    --                                                                   # #     #   #  #  # # # #      #
    --                                                                 ##  ### ##  ##  ###  #  # #     ###
    , session1 : Session1
    , meaning : Logic.Task Meaning.Trial Meaning.State
    , spellingLvl1 : Logic.Task SpellingLvl1.Trial SpellingLvl1.State
    , cu1 : Logic.Task CU1.Trial CU1.State
    , presentation : Logic.Task Presentation.Trial Presentation.State

    --                                                                  .dP"Y8 888888 .dP"Y8 .dP"Y8 88  dP"Yb  88b 88     oP"Yb.
    --                                                                  `Ybo." 88__   `Ybo." `Ybo." 88 dP   Yb 88Yb88     "' dP'
    --                                                                  o.`Y8b 88""   o.`Y8b o.`Y8b 88 Yb   dP 88 Y88       dP'
    --                                                                  8bodP' 888888 8bodP' 8bodP' 88  YbodP  88  Y8     .d8888
    , translationTask : Logic.Task Translation.Trial Translation.State
    , scrabbleTask : Logic.Task Scrabble.Trial Scrabble.State
    , cuLvl2 : Logic.Task CU2.Trial CU2.State
    , session2 : Session2

    --                                                               .dP"Y8 888888 .dP"Y8 .dP"Y8 88  dP"Yb  88b 88     88888
    --                                                               `Ybo." 88__   `Ybo." `Ybo." 88 dP   Yb 88Yb88       .dP
    --                                                               o.`Y8b 88""   o.`Y8b o.`Y8b 88 Yb   dP 88 Y88     o `Yb
    --                                                               8bodP' 888888 8bodP' 8bodP' 88  YbodP  88  Y8     YbodP
    , spelling3 : Logic.Task Spelling3.Trial Spelling3.State
    , cu3 : Logic.Task CU3.Trial CU3.State
    , synonymTask : E.Experiment
    , session3 : Session3

    --                                                              88""Yb  dP"Yb  .dP"Y8 888888 888888 .dP"Y8 888888
    --                                                              88__dP dP   Yb `Ybo."   88   88__   `Ybo."   88
    --                                                              88"""  Yb   dP o.`Y8b   88   88""   o.`Y8b   88
    --                                                              88      YbodP  8bodP'   88   888888 8bodP'   88
    , cloudWords : Dict.Dict String Bool

    --                                                             .dP"Y8 88  88    db    88""Yb 888888 8888b.
    --                                                             `Ybo." 88  88   dPYb   88__dP 88__    8I  Yb
    --                                                             o.`Y8b 888888  dP__Yb  88"Yb  88""    8I  dY
    --                                                             8bodP' 88  88 dP""""Yb 88  Yb 888888 8888Y"
    , infos : RemoteData Http.Error (Dict.Dict String ExperimentInfo.Task)
    , user : Maybe String
    }


init : Flags -> Url -> Nav.Key -> ( Model, Cmd Msg )
init _ url key =
    let
        route =
            Route.fromUrl url

        ( loadingState, fetchCmd ) =
            Para.attempt5
                { task1 = Meaning.getRecords
                , task2 = SpellingLvl1.getRecords
                , task3 = CU1.getRecords
                , task4 = Presentation.getRecords
                , task5 = ExperimentInfo.getRecords
                , onUpdates = ServerRespondedWithSomeSession1Data
                , onFailure = ServerRespondedWithSomeError
                , onSuccess = ServerRespondedWithAllSession1Data
                }

        ( loadingStateSession2, fetchSession2 ) =
            Para.attempt4
                { task1 = CU2.getRecords
                , task2 = Scrabble.getRecords
                , task3 = Translation.getRecords
                , task4 = ExperimentInfo.getRecords
                , onUpdates = ServerRespondedWithSomeSession2Data
                , onFailure = ServerRespondedWithSomeError
                , onSuccess = ServerRespondedWithAllSession2Data
                }

        ( loadingStateSession3, fetchSession3 ) =
            Para.attempt4
                { task1 = CU3.getRecords
                , task2 = Spelling3.getRecords
                , task3 = Synonym.getRecords
                , task4 = ExperimentInfo.getRecords
                , onUpdates = ServerRespondedWithSomeSession3Data
                , onFailure = ServerRespondedWithSomeError
                , onSuccess = ServerRespondedWithAllSession3Data
                }

        defaultInit =
            { key = key
            , route = route
            , dnd = system.model

            -- SESSION 1
            , meaning = Logic.NotStarted
            , spellingLvl1 = Logic.NotStarted
            , cu1 = Logic.Loading
            , presentation = Logic.NotStarted
            , session1 = NotAsked

            -- SESSION 2
            , translationTask = Logic.NotStarted
            , cuLvl2 = Logic.NotStarted
            , scrabbleTask = Logic.NotStarted
            , session2 = NotAskedSession2

            -- SESSION 3
            , synonymTask = E.NotStarted
            , spelling3 = Logic.NotStarted
            , cu3 = Logic.NotStarted
            , session3 = NotAskedSession3

            -- PRETEST
            , yn = Logic.Loading
            , acceptabilityTask = Acceptability.NotStarted

            -- POSTEST
            , cloudWords = Dict.fromList CloudWords.words

            -- SHARED
            , user = url.query
            , optionsOrder = [ 0, 1, 2, 3 ]
            , infos = RemoteData.Loading
            }
    in
    case route of
        Route.Session1 _ ->
            ( { defaultInit
                | -- SESSION 1
                  meaning = Logic.Loading
                , spellingLvl1 = Logic.Loading
                , cu1 = Logic.Loading
                , presentation = Logic.Loading
                , user = Nothing
                , session1 = Loading loadingState
              }
            , fetchCmd
            )

        Route.Home ->
            ( { defaultInit
                | -- SESSION 1
                  meaning = Logic.Loading
                , spellingLvl1 = Logic.Loading
                , cu1 = Logic.Loading
                , presentation = Logic.Loading
                , user = Nothing
                , session1 = Loading loadingState
              }
            , fetchCmd
            )

        Route.AuthenticatedSession2 userid _ ->
            ( { defaultInit
                | -- SESSION 2
                  translationTask = Logic.Loading
                , cuLvl2 = Logic.Loading
                , scrabbleTask = Logic.Loading
                , user = Just userid
                , session2 = LoadingSession2 loadingStateSession2
              }
            , fetchSession2
            )

        Route.AuthenticatedSession3 userid _ ->
            ( { defaultInit
                | -- SESSION 3
                  synonymTask = E.Loading
                , spelling3 = Logic.Loading
                , cu3 = Logic.Loading
                , session3 = LoadingSession3 loadingStateSession3
              }
            , fetchSession3
            )

        _ ->
            Debug.todo ""


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


view : Model -> Browser.Document Msg
view model =
    { title = project.title
    , body = [ body model |> div [] |> toUnstyled ]
    }



--Acceptability ->
--[ Acceptability.view model.acceptabilityTask { nextTrialMsg = Acceptability Acceptability.UserClickedNextTrial } ]


body : Model -> List (Html Msg)
body model =
    let
        infos =
            case model.infos of
                RemoteData.Success infos_ ->
                    infos_

                _ ->
                    Dict.empty
    in
    [ View.header
        [ navIn "L'expérience" "/meaning"
        , navOut "BCL" "https://bcl.cnrs.fr/"
        , navOut "L'équipe" "https://bcl.cnrs.fr/rubrique225"
        ]
    , View.container <|
        case model.route of
            Route.Session1 task ->
                case task of
                    Route.Meaning ->
                        let
                            infos_ =
                                Dict.get "Meaning" infos
                        in
                        [ Meaning.view
                            { task = model.meaning
                            , infos = infos_
                            , radioMsg = \val -> Meaning (Meaning.UserClickedRadioButton val)
                            , toggleFeedbackMsg = Meaning Meaning.UserClickedToggleFeedback
                            , nextTrialMsg = Meaning Meaning.UserClickedNextTrial
                            , optionsOrder = model.optionsOrder
                            , startMainMsg = \trials informations -> Meaning (Meaning.UserClickedStartMain trials informations)
                            , saveDataMsg = Meaning Meaning.SaveDataMsg
                            }
                        ]

                    Route.Presentation ->
                        let
                            synonymInfos =
                                Dict.get "Presentation" infos
                        in
                        [ Presentation.view
                            { task = model.presentation
                            , infos = synonymInfos
                            , nextTrialMsg = Presentation Presentation.UserClickedNextTrial
                            , startMainMsg = \trials informations -> Presentation (Presentation.UserClickedStartMain trials informations)
                            , userClickedAudio = PlaysoundInJS
                            , userToggledElementOfEntry = \entryId -> Presentation (Presentation.UserToggleElementOfEntry entryId)
                            }
                        ]

                    SpellingLevel1 ->
                        [ SpellingLvl1.view model.spellingLvl1
                            model.optionsOrder
                            { toggleFeedbackMsg = Spelling1 SpellingLvl1.UserClickedFeedback
                            , nextTrialMsg = Spelling1 SpellingLvl1.UserClickedNextTrial
                            , radioMsg = \choice -> Spelling1 (SpellingLvl1.UserClickedRadioButton choice)
                            , startMainloopMsg = \trials informations -> Spelling1 (SpellingLvl1.UserClickedStartMainloop trials informations)
                            , playAudio = PlaysoundInJS
                            , saveDataMsg = Spelling1 SpellingLvl1.UserClickedSavedData
                            }
                        ]

                    Route.CU1 ->
                        let
                            cu1Infos =
                                Dict.get "Context Understanding level 1" infos
                        in
                        [ CU1.view
                            { task = model.cu1
                            , infos = cu1Infos
                            , optionsOrder = model.optionsOrder
                            , nextTrialMsg = CU1 CU1.UserClickedNextTrial
                            , radioMsg = \id -> CU1 (CU1.UserClickedRadioButton id)
                            , toggleFeedbackMsg = CU1 CU1.UserClickedToggleFeedback
                            , startMainMsg = \trials informations -> CU1 (CU1.UserClickedStartMain trials informations)
                            , userClickedSaveData = CU1 CU1.UserClickedSaveData
                            }
                        ]

            Route.AuthenticatedSession2 _ task ->
                case task of
                    CULevel2 ->
                        let
                            cu2Infos =
                                Dict.get "Context Understanding level 2" infos
                        in
                        [ CU2.view model.cuLvl2
                            model.optionsOrder
                            { userClickedNextTrial = CU2 CU2.UserClickedNextTrial
                            , userClickedAudio = PlaysoundInJS
                            , radioMsg = \id -> CU2 (CU2.UserClickedRadioButton id)
                            , toggleFeedback = CU2 CU2.UserClickedToggleFeedback
                            , nextTrialMsg = CU2 CU2.UserClickedNextTrial
                            , startMainMsg = \trials informations -> CU2 (CU2.UserClickedStartMain trials informations)
                            , saveData = CU2 CU2.UserClickedSaveData
                            }
                        ]

                    Route.Translation ->
                        let
                            infos_ =
                                Dict.get "Translation" infos
                        in
                        [ Translation.view
                            { task = model.translationTask
                            , infos = infos_
                            , radioMsg = \val -> Translation (Translation.UserClickedRadioButton val)
                            , toggleFeedbackMsg = Translation Translation.UserClickedToggleFeedback
                            , nextTrialMsg = Translation Translation.UserClickedNextTrial
                            , optionsOrder = model.optionsOrder
                            , startMainMsg = \trials informations -> Translation (Translation.UserClickedStartMain trials informations)
                            , saveDataMsg = Translation Translation.UserClickedSaveData
                            }
                        ]

                    Route.Scrabble ->
                        let
                            info =
                                Dict.get "Spelling level 2" infos
                        in
                        viewScrabbleTask model

            Route.AuthenticatedSession3 _ task ->
                case task of
                    Route.CU3 ->
                        let
                            cu3Infos =
                                Dict.get "Context Understanding level 3" infos
                        in
                        [ CU3.view model.cu3
                            { userClickedAudio = PlaysoundInJS
                            , toggleFeedback = CU3 CU3.UserClickedToggleFeedback
                            , nextTrialMsg = CU3 CU3.UserClickedNextTrial
                            , startMainMsg = \trials informations -> CU3 (CU3.UserClickedStartMain trials informations)
                            , userChangedInput = \new -> CU3 (CU3.UserChangedInput new)
                            , saveDataMsg = CU3 CU3.UserClickedSaveData
                            }
                        ]

                    Route.Synonym ->
                        let
                            synonymInfos =
                                Dict.get "Synonym" infos
                        in
                        Synonym.viewTask model.synonymTask
                            synonymInfos
                            { toggleFeedbackMsg = Synonym Synonym.UserClickedFeedback
                            , inputValidationMsg = Synonym Synonym.UserValidatedInput
                            , nextTrialMsg = Synonym Synonym.UserClickedNextTrial
                            , toMainloopMsg = \trials -> Synonym (Synonym.UserClickedStartMainloop trials)
                            , updateInputMsg = \input -> Synonym (Synonym.UserChangedInput input)
                            }

                    Route.Spelling3 ->
                        let
                            infos_ =
                                Dict.get "Spelling level 3" infos
                        in
                        [ Spelling3.view model.spelling3
                            { userClickedAudio = PlaysoundInJS
                            , toggleFeedback = Spelling3 Spelling3.UserClickedToggleFeedback
                            , nextTrialMsg = Spelling3 Spelling3.UserClickedNextTrial
                            , startMainMsg = \trials informations -> Spelling3 (Spelling3.UserClickedStartMain trials informations)
                            , userChangedInput = \new -> Spelling3 (Spelling3.UserChangedInput new)
                            }
                        ]

            Route.Posttest task ->
                case task of
                    Route.CloudWords ->
                        [ viewCloud model ]

            Route.Pretest task ->
                case task of
                    Route.YN ->
                        let
                            infos_ =
                                Dict.get "YesNo task" infos
                        in
                        [ YN.view model.yn
                            { toggleFeedback = YN YN.UserClickedToggleFeedback
                            , nextTrialMsg = YN YN.UserClickedNextTrial
                            , startMainMsg = \trials informations -> YN (YN.UserClickedStartMain trials informations)
                            , userChangedInput = \str -> YN (YN.UserChangedInput str)
                            }
                        ]

            Home ->
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
                            |> Dict.toList
                            |> List.map Tuple.second
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
                , viewCard (\info -> info.session == ExperimentInfo.Session1)
                ]

            NotFound ->
                View.notFound
    ]



--words : List (Html msg)
-- UPDATE
{--Meaning.getRecords
                        , task2 = SpellingLvl1.getRecords
                        , task3 = CU1.getRecords
                        , task4 = Presentation.getRecords
                        , task5 = ExperimentInfo.getRecords
                        , onUpdate = ServerRespondedWithSomeSession1Data
                        , onFailure = ServerRespondedWithSomeError
                        , onSuccess = ServerRespondedWithAllSession1Data} --}


type Msg
    = ServerRespondedWithSynonymTrials (Result Http.Error (List E.SynonymTrial))
    | ServerRespondedWithAcceptabilityTrials (Result Http.Error (List Acceptability.Trial))
    | ServerRespondedWithUserInfo (Result Http.Error User.AuthenticatedInfo)
    | UserClickedStartSynonym (List E.SynonymTrial)
    | UserToggledInCloudWords String
      --
      --                                                           # # ### ### #    ##
      --                                                           # #  #   #  #   #
      --                                                           # #  #   #  #    #
      --                                                           # #  #   #  #     #
      --                                                           ###  #  ### ### ##
    | PlaysoundInJS String
    | WithTime Msg Time.Posix
    | RuntimeShuffledOptionsOrder (List Int)
    | Shuffled Msg
    | UserDragsLetter DnDList.Msg
    | UserClickedLink Browser.UrlRequest
    | BrowserChangedUrl Url
      --
      --                                                          ##  ##  ### ### ###  ## ###
      --                                                          # # # # #    #  #   #    #
      --                                                          ##  ##  ##   #  ##   #   #
      --                                                          #   # # #    #  #     #  #
      --                                                          #   # # ###  #  ### ##   #
    | Acceptability Acceptability.Msg
    | UserPressedKey Acceptability.Evaluation
      --
      --                                                          ## ###  ##  ## ###  #  ###      #
      --                                                         #   #   #   #    #  # # # #     ##
      --                                                          #  ##   #   #   #  # # # #      #
      --                                                           # #     #   #  #  # # # #      #
      --                                                         ##  ### ##  ##  ###  #  # #     ###
    | CU1 CU1.CU1Msg
    | Presentation Presentation.Msg
    | Meaning Meaning.Msg
    | Spelling1 SpellingLvl1.Msg
    | ServerRespondedWithSomeSession1Data (Para.Msg5 (List Meaning.Trial) (List SpellingLvl1.Trial) (List CU1.Trial) (List Presentation.Trial) (List ExperimentInfo.Task))
    | ServerRespondedWithSomeError Http.Error
    | ServerRespondedWithAllSession1Data (List Meaning.Trial) (List SpellingLvl1.Trial) (List CU1.Trial) (List Presentation.Trial) (List ExperimentInfo.Task)
      --
      --                                                          ## ###  ##  ## ###  #  ###     ###
      --                                                         #   #   #   #    #  # # # #       #
      --                                                          #  ##   #   #   #  # # # #     ###
      --                                                           # #     #   #  #  # # # #     #
      --                                                         ##  ### ##  ##  ###  #  # #     ###
    | CU2 CU2.CU2Msg
    | Spelling2 Spelling2Msg
    | Translation Translation.Msg
    | ServerRespondedWithSomeSession2Data (Para.Msg4 (List CU2.Trial) (List Scrabble.Trial) (List Translation.Trial) (List ExperimentInfo.Task))
    | ServerRespondedWithAllSession2Data (List CU2.Trial) (List Scrabble.Trial) (List Translation.Trial) (List ExperimentInfo.Task)
      --
      --                                                           ## ###  ##  ## ###  #  ###     ###
      --                                                          #   #   #   #    #  # # # #       #
      --                                                           #  ##   #   #   #  # # # #      ##
      --                                                            # #     #   #  #  # # # #       #
      --                                                          ##  ### ##  ##  ###  #  # #     ###
    | CU3 CU3.Msg
    | Spelling3 Spelling3.Msg
    | YN YN.Msg
    | Synonym Synonym.Msg
    | ServerRespondedWithSomeSession3Data (Para.Msg4 (List CU3.Trial) (List Spelling3.Trial) (List E.SynonymTrial) (List ExperimentInfo.Task))
    | ServerRespondedWithAllSession3Data (List CU3.Trial) (List Spelling3.Trial) (List E.SynonymTrial) (List ExperimentInfo.Task)



--| GetTimeAndThen (Time.Posix -> Msg)


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    let
        --currentMeaningState =
        --    Logic.getState model.meaning
        --currentTranslationState =
        --  Logic.getState model.translationTask
        currentSynonymState =
            case E.getState model.synonymTask of
                E.SynonymStateType x ->
                    x

                _ ->
                    Synonym.initState

        currentScrabbleState =
            case Logic.getState model.scrabbleTask of
                Just x ->
                    x

                _ ->
                    Scrabble.initState

        currentSpellingState =
            Logic.getState model.spellingLvl1

        --infos id =
        --  RemoteData.unwrap (Result.Err "I tried to unwrap the data contained in the tasks' infos but something is stopping me. Maybe the data were still loading?") (\info -> Dict.get id info |> Result.fromMaybe ("I couldn't fetch the value associated with: " ++ id)) model.infos
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

        ServerRespondedWithSomeSession1Data downloadMsg ->
            let
                ( updte, cmd ) =
                    case model.session1 of
                        Loading downloadState ->
                            Para.update5 downloadState downloadMsg |> Tuple.mapFirst Loading

                        _ ->
                            ( model.session1, Cmd.none )
            in
            ( { model | session1 = updte }, cmd )

        ServerRespondedWithSomeError _ ->
            ( model, Cmd.none )

        ServerRespondedWithAllSession1Data meaning spelling cu1 presentation infos_ ->
            ( { model
                | meaning = Meaning.start infos_ meaning
                , spellingLvl1 = SpellingLvl1.start infos_ spelling
                , cu1 = CU1.start infos_ cu1
                , presentation = Presentation.start infos_ presentation
                , infos = RemoteData.Success (ExperimentInfo.toDict infos_)
                , session1 = Ready
              }
            , Cmd.none
            )

        ServerRespondedWithSomeSession2Data downloadMsg ->
            let
                ( updte, cmd ) =
                    case model.session2 of
                        LoadingSession2 downloadState ->
                            Para.update4 downloadState downloadMsg |> Tuple.mapFirst LoadingSession2

                        _ ->
                            ( model.session2, Cmd.none )
            in
            ( { model | session2 = updte }, cmd )

        ServerRespondedWithAllSession2Data cu spelling translation infos_ ->
            let
                shuffleLetters =
                    --shuffleLetters recreates each trial in shuffling every each word's letter. When it's done it shuffles trials.
                    spelling
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
            ( { model
                | translationTask = Translation.start infos_ translation
                , cuLvl2 = CU2.start infos_ cu
                , scrabbleTask = Scrabble.start infos_ spelling
                , session2 = ReadySession2
              }
            , Cmd.none
              -- ( model, Random.generate (\shuffledData -> Shuffled (ServerRespondedWithScrabbleTrials (Result.Ok shuffledData))) shuffleLetters
            )

        ServerRespondedWithSomeSession3Data downloadMsg ->
            let
                ( updte, cmd ) =
                    case model.session3 of
                        LoadingSession3 downloadState ->
                            Para.update4 downloadState downloadMsg |> Tuple.mapFirst LoadingSession3

                        _ ->
                            ( model.session3, Cmd.none )
            in
            ( { model | session3 = updte }, cmd )

        ServerRespondedWithAllSession3Data _ _ _ _ ->
            Debug.todo ""

        Shuffled message ->
            case message of
                ServerRespondedWithSynonymTrials (Result.Ok data) ->
                    ( { model | synonymTask = E.DoingSynonym (E.Intro data Synonym.initState 0 False "Instructions de synonyme") }, Cmd.none )

                ServerRespondedWithAcceptabilityTrials (Result.Ok trials) ->
                    ( { model | acceptabilityTask = Acceptability.DoingTask trials Acceptability.initState 0 [] }, Cmd.none )

                ServerRespondedWithAllSession2Data cu spelling translation infos_ ->
                    let
                        trainingItems =
                            List.filter (\datum -> datum.isTraining) spelling

                        mainItems =
                            List.filter (\datum -> not datum.isTraining) spelling

                        firstTrialWord =
                            trainingItems
                                |> List.head
                                |> Maybe.withDefault Scrabble.defaultTrial
                                |> .writtenWord
                    in
                    ( { model
                        | scrabbleTask = model.scrabbleTask

                        --Logic.startIntro (infos "recSL8cthViyXRx8u") trainingItems mainItems { currentScrabbleState | userAnswer = firstTrialWord, scrambledLetter = toItems firstTrialWord }
                      }
                    , Cmd.none
                    )

                _ ->
                    ( model, Cmd.none )

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

        ServerRespondedWithUserInfo (Result.Ok _) ->
            --( { model | user = Just (User.Authenticated info) }, User.encoder (User.Authenticated info) |> Encode.encode 0 |> User.storeInfo )
            ( model, Cmd.none )

        ServerRespondedWithUserInfo (Result.Err _) ->
            ( model, Cmd.none )

        ServerRespondedWithAcceptabilityTrials (Result.Err reason) ->
            ( { model | acceptabilityTask = Acceptability.Failure reason }
            , Cmd.none
            )

        Acceptability _ ->
            Debug.todo "acceptability messages"

        UserClickedStartSynonym trials ->
            ( { model | synonymTask = E.DoingSynonym (E.MainLoop trials Synonym.initState 0 False) }, Cmd.none )

        UserPressedKey evaluation ->
            ( model
            , Task.perform (\time -> WithTime (UserPressedKey evaluation) time) Time.now
            )

        UserToggledInCloudWords word ->
            ( { model | cloudWords = CloudWords.toggle word model.cloudWords }, Cmd.none )

        UserDragsLetter dndmsg ->
            let
                --items_ =
                --  toItems currentScrabbleState.userAnswer
                ( dnd, items ) =
                    system.update dndmsg model.dnd currentScrabbleState.scrambledLetter
            in
            ( { model
                | dnd = dnd
                , scrabbleTask = Logic.update { currentScrabbleState | scrambledLetter = items, userAnswer = String.concat (List.map Tuple.second items) } model.scrabbleTask
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

        PlaysoundInJS url ->
            ( model, playAudio url )

        Spelling1 message ->
            let
                taskId =
                    "recJOpE5pMTCHJOSV"
            in
            case message of
                SpellingLvl1.UserClickedFeedback ->
                    ( { model
                        | spellingLvl1 =
                            model.spellingLvl1
                                |> Logic.toggle
                      }
                    , Cmd.none
                    )

                SpellingLvl1.UserClickedRadioButton newChoice ->
                    case currentSpellingState of
                        Just prevState ->
                            ( { model
                                | spellingLvl1 =
                                    Logic.update { prevState | userAnswer = newChoice } model.spellingLvl1
                              }
                            , Cmd.none
                            )

                        Nothing ->
                            ( model, Cmd.none )

                SpellingLvl1.UserClickedNextTrial ->
                    ( { model | spellingLvl1 = Logic.next SpellingLvl1.initState model.spellingLvl1 }, Cmd.none )

                SpellingLvl1.UserClickedStartMainloop trials infos ->
                    ( { model | spellingLvl1 = Logic.startMain infos trials SpellingLvl1.iniState }, Cmd.none )

                SpellingLvl1.UserClickedSavedData ->
                    let
                        responseHandler =
                            \records -> Spelling1 (SpellingLvl1.ServerRespondedWithLastRecords records)
                    in
                    ( model, Logic.saveData responseHandler model.user taskId model.spellingLvl1 )

                SpellingLvl1.ServerRespondedWithLastRecords _ ->
                    ( model, Cmd.none )

        CU2 message ->
            let
                taskId =
                    "recwxsmowpB18bpLj"
            in
            case message of
                CU2.UserClickedNextTrial ->
                    ( { model | cuLvl2 = Logic.next CU2.initState model.cuLvl2 }, Random.generate RuntimeShuffledOptionsOrder (Random.List.shuffle model.optionsOrder) )

                CU2.UserClickedToggleFeedback ->
                    ( { model | cuLvl2 = Logic.toggle model.cuLvl2 }, Cmd.none )

                CU2.UserClickedRadioButton newChoice ->
                    ( { model | cuLvl2 = Logic.update { uid = "", userAnswer = newChoice } model.cuLvl2 }, Cmd.none )

                CU2.UserClickedStartMain trials infos ->
                    ( { model | cuLvl2 = Logic.startMain infos trials CU2.initState }, Cmd.none )

                CU2.ServerRespondedWithLastRecords _ ->
                    ( model, Cmd.none )

                CU2.UserClickedSaveData ->
                    let
                        responseHandler =
                            \records -> CU2 (CU2.ServerRespondedWithLastRecords records)
                    in
                    ( model, Logic.saveData responseHandler model.user taskId model.cuLvl2 )

        Spelling2 message ->
            let
                taskId =
                    "recSL8cthViyXRx8u"
            in
            case message of
                UserClickedFeedbackButton ->
                    ( { model | scrabbleTask = Logic.toggle model.scrabbleTask }, Cmd.none )

                UserClickedStartButton ->
                    ( { model | scrabbleTask = Logic.toggle model.scrabbleTask }, Cmd.none )

                UserClickedNextTrial (Just nextTrial) ->
                    ( { model
                        | scrabbleTask =
                            Logic.next
                                { currentScrabbleState
                                    | userAnswer = nextTrial.writtenWord
                                    , scrambledLetter = toItems nextTrial.writtenWord
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
                            \records -> Spelling2 (ServerRespondedWithLastRecords records)
                    in
                    ( model, Logic.saveData responseHandler model.user taskId model.scrabbleTask )

                ServerRespondedWithLastRecords _ ->
                    ( model, Cmd.none )

                UserClickedStartMainloop trials infos ->
                    case trials of
                        [] ->
                            ( { model | scrabbleTask = Logic.Err "You gave no trial to start the main loop. Please report this error message." }, Cmd.none )

                        x :: _ ->
                            ( { model | scrabbleTask = Logic.startMain infos trials { currentScrabbleState | userAnswer = x.writtenWord, scrambledLetter = toItems x.writtenWord } }, Cmd.none )

        CU1 message ->
            let
                taskId =
                    "recsN8oyy3LIC8URx"
            in
            case message of
                CU1.UserClickedNextTrial ->
                    ( { model | cu1 = Logic.next CU1.initState model.cu1 }, Random.generate RuntimeShuffledOptionsOrder (Random.List.shuffle model.optionsOrder) )

                CU1.UserClickedToggleFeedback ->
                    ( { model | cu1 = Logic.toggle model.cu1 }, Cmd.none )

                CU1.UserClickedRadioButton newChoice ->
                    ( { model | cu1 = Logic.update { uid = "", userAnswer = newChoice } model.cu1 }, Cmd.none )

                CU1.UserClickedStartMain trials infos ->
                    ( { model | cu1 = Logic.startMain infos trials CU2.initState }, Cmd.none )

                CU1.UserClickedSaveData ->
                    let
                        responseHandler =
                            \records -> CU1 (CU1.ServerRespondedWithLastRecords records)
                    in
                    ( model, Logic.saveData responseHandler model.user taskId model.cu1 )

                CU1.ServerRespondedWithLastRecords (Result.Ok _) ->
                    ( model, Cmd.none )

                CU1.ServerRespondedWithLastRecords (Err _) ->
                    ( model, Cmd.none )

        CU3 message ->
            let
                taskId =
                    "recFEtKbtuBSolHnI"
            in
            case message of
                CU3.UserClickedNextTrial ->
                    ( { model | cu3 = Logic.next CU1.initState model.cu3 }, Random.generate RuntimeShuffledOptionsOrder (Random.List.shuffle model.optionsOrder) )

                CU3.UserClickedToggleFeedback ->
                    ( { model | cu3 = Logic.toggle model.cu3 }, Cmd.none )

                CU3.UserClickedRadioButton newChoice ->
                    ( { model | cu3 = Logic.update { uid = "", userAnswer = newChoice } model.cu3 }, Cmd.none )

                CU3.UserClickedStartMain trials infos ->
                    ( { model | cu3 = Logic.startMain infos trials CU3.initState }, Cmd.none )

                CU3.UserChangedInput new ->
                    ( { model | cu3 = Logic.update { uid = "", userAnswer = new } model.cu3 }, Cmd.none )

                CU3.UserClickedSaveData ->
                    let
                        responseHandler =
                            \records -> CU3 (CU3.ServerRespondedWithLastRecords records)
                    in
                    ( model, Logic.saveData responseHandler model.user taskId model.scrabbleTask )

                CU3.ServerRespondedWithLastRecords _ ->
                    ( model, Cmd.none )

        Spelling3 message ->
            let
                taskId =
                    "recJucOXEZzJj6Uui"
            in
            case message of
                Spelling3.UserClickedNextTrial ->
                    ( { model | spelling3 = Logic.next CU1.initState model.spelling3 }, Cmd.none )

                Spelling3.UserClickedToggleFeedback ->
                    ( { model | spelling3 = Logic.toggle model.spelling3 }, Cmd.none )

                Spelling3.UserClickedStartIntro _ ->
                    Debug.todo ""

                Spelling3.UserClickedStartMain trials infos ->
                    ( { model | spelling3 = Logic.startMain infos trials Spelling3.initState }, Cmd.none )

                Spelling3.UserChangedInput new ->
                    ( { model | spelling3 = Logic.update { uid = "", userAnswer = new } model.spelling3 }, Cmd.none )

        YN message ->
            let
                taskId =
                    "rechYdq4MyLcb2nRG"
            in
            case message of
                YN.UserClickedNextTrial ->
                    ( { model | yn = Logic.next CU1.initState model.yn }, Cmd.none )

                YN.UserClickedToggleFeedback ->
                    ( { model | yn = Logic.toggle model.yn }, Cmd.none )

                YN.UserClickedStartIntro _ ->
                    Debug.todo ""

                YN.UserClickedStartMain trials infos ->
                    ( { model | yn = Logic.startMain infos trials YN.initState }, Cmd.none )

                YN.UserChangedInput new ->
                    ( { model | yn = Logic.update { uid = "", userAnswer = new } model.yn }, Cmd.none )

        Presentation message ->
            let
                taskId =
                    "rechYdq4MyLcb2nRG"
            in
            case message of
                Presentation.UserClickedNextTrial ->
                    ( { model | presentation = Logic.next Presentation.initState model.presentation }, Cmd.none )

                Presentation.UserClickedStartIntro _ ->
                    Debug.todo "Start intro"

                Presentation.UserClickedStartMain trials infos ->
                    ( { model | presentation = Logic.startMain infos trials Presentation.initState }, Cmd.none )

                Presentation.UserToggleElementOfEntry id ->
                    let
                        prevState =
                            Logic.getState model.presentation
                    in
                    case prevState of
                        Just state ->
                            ( { model | presentation = Logic.update { state | toggledEntries = Dict.update id (Maybe.map not) state.toggledEntries } model.presentation }, Cmd.none )

                        Nothing ->
                            ( model, Cmd.none )

        Synonym message ->
            case message of
                Synonym.UserClickedFeedback ->
                    ( { model
                        | synonymTask =
                            model.synonymTask
                                |> E.toggleFeedback
                      }
                    , Cmd.none
                    )

                Synonym.UserChangedInput newChoice ->
                    ( { model
                        | synonymTask =
                            model.synonymTask
                                |> E.updateState (E.SynonymStateType { currentSynonymState | userAnswer = newChoice })
                      }
                    , Cmd.none
                    )

                _ ->
                    Debug.todo "Missing Msg variants in Synonym "

        Meaning message ->
            let
                taskId =
                    "rec9fDmVOpqDJktmQ"
            in
            case message of
                Meaning.UserClickedNextTrial ->
                    ( { model | meaning = Logic.next Meaning.initState model.meaning }, Random.generate RuntimeShuffledOptionsOrder (Random.List.shuffle model.optionsOrder) )

                Meaning.UserClickedToggleFeedback ->
                    ( { model | meaning = Logic.toggle model.meaning }, Cmd.none )

                Meaning.UserClickedRadioButton newChoice ->
                    ( { model | meaning = Logic.update { uid = "", userAnswer = newChoice } model.meaning }, Cmd.none )

                Meaning.UserClickedStartIntro _ ->
                    ( model, Cmd.none )

                Meaning.UserClickedStartMain trials infos ->
                    ( { model | meaning = Logic.startMain infos trials Meaning.initState }, Cmd.none )

                Meaning.SaveDataMsg ->
                    let
                        responseHandler =
                            \records -> Meaning (Meaning.ServerRespondedWithLastRecords records)
                    in
                    ( model, Logic.saveData responseHandler model.user taskId model.translationTask )

                Meaning.ServerRespondedWithLastRecords _ ->
                    ( model, Cmd.none )

        Translation message ->
            let
                taskId =
                    "recf5HANE632FLKbc"
            in
            case message of
                Translation.UserClickedNextTrial ->
                    ( { model | translationTask = Logic.next Translation.initState model.translationTask }
                    , Cmd.batch
                        [ Random.generate RuntimeShuffledOptionsOrder (Random.List.shuffle model.optionsOrder)
                        ]
                    )

                Translation.ServerRespondedWithLastRecords (Ok _) ->
                    ( model, Cmd.none )

                Translation.ServerRespondedWithLastRecords (Err _) ->
                    ( model, Cmd.none )

                Translation.RuntimeSentData _ ->
                    ( model, Cmd.none )

                Translation.UserClickedToggleFeedback ->
                    ( { model | translationTask = Logic.toggle model.translationTask }, Cmd.none )

                Translation.UserClickedRadioButton newChoice ->
                    ( { model | translationTask = Logic.update { uid = "", userAnswer = newChoice } model.translationTask }, Cmd.none )

                Translation.UserClickedStartIntro _ ->
                    ( model, Cmd.none )

                Translation.UserClickedStartMain trials infos ->
                    ( { model | translationTask = Logic.startMain infos trials Translation.initState }, Cmd.none )

                Translation.UserClickedSaveData ->
                    let
                        responseHandler =
                            \records -> CU1 (CU1.ServerRespondedWithLastRecords records)
                    in
                    ( model, Logic.saveData responseHandler model.user taskId model.translationTask )


toItems : String -> List E.KeyedItem
toItems string =
    string
        |> String.toList
        |> List.map String.fromChar
        |> toKeyedItem


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


green : String
green =
    "#3da565"


ghostGreen : String
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



--
--                                                                   ## ##  ### #   #   ### ###  ## ###
--                                                                  #   # # #   #   #    #  # # #     #
--                                                                   #  ##  ##  #   #    #  # # # # ###
--                                                                    # #   #   #   #    #  # # # # #
--                                                                  ##  #   ### ### ### ### # #  ## ###


type Spelling2Msg
    = UserClickedFeedbackButton
    | UserClickedNextTrial (Maybe Scrabble.Trial)
    | UserClickedStartButton
    | UserClickedStartMainloop (List Scrabble.Trial) Task
    | UserClickedSaveData
    | ServerRespondedWithLastRecords (Result Http.Error (List String))


viewScrabbleTask : { a | scrabbleTask : Logic.Task Scrabble.Trial Scrabble.State, dnd : DnDList.Model, route : Route } -> List (Html Msg)
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

        feedback pre_correct pre_incorrect feedback_ { target, attempt } nextItem =
            case ( feedback_, target == attempt ) of
                ( True, True ) ->
                    div [ class " rounded-md text-center object-center bg-green-300 m-8" ]
                        [ p [ class "p-6 text-xl text-white" ]
                            [ text pre_correct
                            , span [ class "font-bold" ] [ text target ]
                            ]
                        , div [ class "pb-4" ]
                            [ View.button
                                { message = Spelling2 (UserClickedNextTrial nextItem)
                                , txt = "Next"
                                , isDisabled = False
                                }
                            ]
                        ]

                ( True, False ) ->
                    div [ class " rounded-md text-center object-center bg-red-300 m-8" ]
                        [ p [ class "p-6 text-xl text-white" ]
                            [ text pre_incorrect
                            , span [ class "font-bold" ] [ text target ]
                            ]
                        , div [ class "pb-4" ]
                            [ View.button
                                { message = Spelling2 (UserClickedNextTrial nextItem)
                                , txt = "Next"
                                , isDisabled = False
                                }
                            ]
                        ]

                _ ->
                    div []
                        [ View.button
                            { message = Spelling2 UserClickedFeedbackButton
                            , txt = "Check my answer"
                            , isDisabled = False
                            }
                        ]
    in
    case model.scrabbleTask of
        Logic.NotStarted ->
            [ text "Not Asked" ]

        Logic.Main data ->
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
                        data.infos.feedback_correct
                        data.infos.feedback_incorrect
                        data.feedback
                        { target = currentTrial.target, attempt = data.state.userAnswer }
                        data.next
                    ]

                Nothing ->
                    [ View.end data.infos.end (Spelling2 UserClickedSaveData) ]

        Logic.Intr data ->
            case data.current of
                Just currentTrial ->
                    [ View.viewTraining data.infos.instructions
                        [ audioButton currentTrial.audioWord.url
                        , div [ class "col-start-2 col-span-4" ] [ viewLetters data.state.scrambledLetter ]
                        , ghostView model.dnd data.state.scrambledLetter
                        , div [ class "col-start-2 col-span-4 pb-4" ]
                            [ feedback
                                data.infos.feedback_correct
                                data.infos.feedback_incorrect
                                data.feedback
                                { target = currentTrial.target, attempt = data.state.userAnswer }
                                data.next
                            ]
                        ]
                    ]

                Nothing ->
                    [ View.introToMain (Spelling2 (UserClickedStartMainloop data.mainTrials data.infos)) ]

        Logic.Loading ->
            [ text "Loading..." ]

        Logic.Err reason ->
            [ text reason ]


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
