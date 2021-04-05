port module Main exposing
    ( isNextSentence
    , main
    , organizeAcceptabilityTrials
    , removesItems
    , toEvaluation
    )

import Browser
import Browser.Dom
import Browser.Events exposing (onKeyDown)
import Browser.Navigation as Nav
import Data
import Delay
import Dict
import DnDList
import DnDList.Groups exposing (Model)
import ExperimentInfo exposing (Session(..), Task)
import Html.Styled exposing (..)
import Html.Styled.Attributes exposing (class, for, href, id, type_)
import Html.Styled.Events
import Html.Styled.Keyed as Keyed
import Http
import Icons
import Json.Decode
import Json.Decode.Pipeline
import Json.Encode
import List.Extra
import Logic
import Postest.CloudWords as CloudWords
import Postest.YN as YN
import Pretest.Acceptability as Acceptability
import Pretest.GeneralInfos
import Random
import Random.Extra
import Random.List exposing (shuffle)
import RemoteData exposing (RemoteData)
import Result exposing (Result)
import Route exposing (Route(..), Session1Task(..), Session2Task(..))
import Session1.CU1 as CU1
import Session1.Meaning as Meaning
import Session1.Presentation as Presentation
import Session1.SpellingLvl1 as SpellingLvl1
import Session1.Top
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


port audioEnded : (InboundAudioInfos -> msg) -> Sub msg


type alias InboundAudioInfos =
    { endedAt : Int, audioName : String }


decodeAudioInfos =
    Json.Decode.succeed InboundAudioInfos
        |> Json.Decode.Pipeline.required "endedAt" Json.Decode.int
        |> Json.Decode.Pipeline.required "audioName" Json.Decode.string


mapAudioInfos json =
    case Json.Decode.decodeValue decodeAudioInfos json of
        Ok decodedAudioInfos ->
            Acceptability (Acceptability.AudioEnded decodedAudioInfos)

        Err reason ->
            let
                _ =
                    Debug.log "Error in mapAudioInfos :" reason
            in
            NoOp



-- MODEL


type Session a
    = Loading a
    | FailedToLoad String
    | Ready
    | NotAsked


type alias Session1 =
    Session (Para.State5 Msg (List Meaning.Trial) (List SpellingLvl1.Trial) (List CU1.Trial) (List Presentation.Trial) (List ExperimentInfo.Task))


type alias Session2 =
    Session (Para.State4 Msg (List CU2.Trial) (List Scrabble.Trial) (List Translation.Trial) (List ExperimentInfo.Task))


type alias Session3 =
    Session (Para.State4 Msg (List CU3.Trial) (List Spelling3.Trial) (List Synonym.Trial) (List ExperimentInfo.Task))


type alias Pilote =
    Session (Para.State2 Msg (List Acceptability.Trial) (List ExperimentInfo.Task))


type alias Model =
    { --                                                                88   88 888888 88 88     .dP"Y8
      --                                                                88   88   88   88 88     `Ybo."
      --                                                                Y8   8P   88   88 88  .o o.`Y8b
      --                                                                `YbodP'   88   88 88ood8 8bodP'
      key : Nav.Key
    , route : Route.Route
    , optionsOrder : List Int

    -- dnd stands for drag-and-drop
    , dnd : DnDList.Model

    --                                                                  88""Yb 88""Yb 888888 888888 888888 .dP"Y8 888888
    --                                                                  88__dP 88__dP 88__     88   88__   `Ybo."   88
    --                                                                  88"""  88"Yb  88""     88   88""   o.`Y8b   88
    --                                                                  88     88  Yb 888888   88   888888 8bodP'   88
    -- yn stands for yes-no
    , yn : Logic.Task YN.Trial YN.State
    , acceptabilityTask : Logic.Task Acceptability.Trial Acceptability.State
    , informations : Pretest.GeneralInfos.Model
    , pilote : Pilote

    --
    --                                                                  ## ###  ##  ## ###  #  ###      #
    --                                                                 #   #   #   #    #  # # # #     ##
    --                                                                  #  ##   #   #   #  # # # #      #
    --                                                                   # #     #   #  #  # # # #      #
    --                                                                 ##  ### ##  ##  ###  #  # #     ###
    , session1 : Session1
    , meaning : Logic.Task Meaning.Trial Meaning.State
    , spellingLvl1 : Logic.Task SpellingLvl1.Trial SpellingLvl1.State

    --cu1 stands for context-understanding-1
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
    , synonymTask : Logic.Task Synonym.Trial Synonym.State
    , session3 : Session3

    --                                                              88""Yb  dP"Yb  .dP"Y8 888888 888888 .dP"Y8 888888
    --                                                              88__dP dP   Yb `Ybo."   88   88__   `Ybo."   88
    --                                                              88"""  Yb   dP o.`Y8b   88   88""   o.`Y8b   88
    --                                                              88      YbodP  8bodP'   88   888888 8bodP'   88
    , cloudWords : Dict.Dict String CloudWords.WordKnowledge

    --                                                             .dP"Y8 88  88    db    88""Yb 888888 8888b.
    --                                                             `Ybo." 88  88   dPYb   88__dP 88__    8I  Yb
    --                                                             o.`Y8b 888888  dP__Yb  88"Yb  88""    8I  dY
    --                                                             8bodP' 88  88 dP""""Yb 88  Yb 888888 8888Y"
    , infos : RemoteData Http.Error (Dict.Dict String ExperimentInfo.Task)
    , user : Maybe String
    , errorTracking : List Http.Error
    , isStudent : Bool
    , whichDegree : String
    , whichLanguages : String
    , age : Int
    , gender : String
    , dominantHand : String
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

        ( loadingStatePilote, fetchPretest ) =
            Para.attempt2
                { task1 = Acceptability.getRecords
                , task2 = ExperimentInfo.getRecords
                , onUpdates = ServerRespondedWithSomePretestData
                , onFailure = ServerRespondedWithSomeError
                , onSuccess = ServerRespondedWithAllPretestData
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
            , session2 = NotAsked

            -- SESSION 3
            , synonymTask = Logic.NotStarted
            , spelling3 = Logic.NotStarted
            , cu3 = Logic.NotStarted
            , session3 = NotAsked

            -- PRETEST
            , yn = Logic.Loading
            , acceptabilityTask = Logic.NotStarted
            , informations = ""
            , pilote = NotAsked

            -- POSTEST
            , cloudWords = Dict.fromList CloudWords.words

            -- SHARED
            , user = url.query
            , optionsOrder = [ 0, 1, 2, 3 ]
            , infos = RemoteData.Loading
            , errorTracking = []
            , isStudent = False
            , whichDegree = ""
            , whichLanguages = ""
            , age = 0
            , gender = ""
            , dominantHand = ""
            }
    in
    case route of
        Route.Session1 userId _ ->
            ( { defaultInit
                | -- SESSION 1
                  meaning = Logic.Loading
                , spellingLvl1 = Logic.Loading
                , cu1 = Logic.Loading
                , presentation = Logic.Loading
                , user = Just userId
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
                , session2 = Loading loadingStateSession2
              }
            , fetchSession2
            )

        Route.AuthenticatedSession3 userid _ ->
            ( { defaultInit
                | -- SESSION 3
                  synonymTask = Logic.Loading
                , spelling3 = Logic.Loading
                , cu3 = Logic.Loading
                , user = Just userid
                , session3 = Loading loadingStateSession3
              }
            , fetchSession3
            )

        Route.Pretest _ ->
            ( { defaultInit
                | yn = Logic.Loading
                , acceptabilityTask = Logic.Loading
                , pilote = Loading loadingStatePilote
              }
            , fetchPretest
            )

        Route.Pilote userid _ ->
            ( { defaultInit
                | acceptabilityTask = Logic.Loading
                , pilote = Loading loadingStatePilote
                , user = Just userid
              }
            , fetchPretest
            )

        Posttest _ ->
            pure defaultInit

        NotFound ->
            pure { defaultInit | route = NotFound }


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
        [ navOut "BCL" "https://bcl.cnrs.fr/"
        , navOut "L'équipe" "https://bcl.cnrs.fr/rubrique225"
        ]
    , View.container <|
        case model.route of
            Route.Session1 _ task ->
                case task of
                    Route.TopSession1 ->
                        Session1.Top.view infos

                    Route.Meaning ->
                        let
                            infos_ =
                                Dict.get "Meaning" infos
                        in
                        [ Meaning.view
                            { task = model.meaning
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
                            , saveDataMsg = NoOp
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
                                Dict.get "recB3kUQW4jNTlou6" infos
                        in
                        Synonym.viewTask model.synonymTask
                            { toggleFeedbackMsg = Synonym Synonym.UserClickedFeedback
                            , nextTrialMsg = Synonym Synonym.UserClickedNextTrial
                            , toMainloopMsg = \trials inf -> Synonym (Synonym.UserClickedStartMainloop trials inf)
                            , updateInputMsg = \input -> Synonym (Synonym.UserChangedInput input)
                            , saveDataMsg = Synonym Synonym.SaveDataMsg
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
                            , saveData = Spelling3 Spelling3.UserClickedSaveData
                            }
                        ]

            Route.Posttest task ->
                case task of
                    Route.CloudWords ->
                        [ viewCloud model ]

            Route.Pilote _ task ->
                case task of
                    Route.AcceptabilityStart ->
                        Acceptability.view model.acceptabilityTask
                            { startTraining = Acceptability Acceptability.StartTraining
                            , startMainMsg = \informations trials -> Acceptability (Acceptability.StartMain trials informations)
                            , saveDataMsg = Acceptability Acceptability.UserClickedSaveMsg
                            }

                    Route.AcceptabilityInstructions ->
                        case model.infos of
                            RemoteData.Success informations ->
                                let
                                    taskInfo =
                                        Dict.get "recR8areYkKRvQ6lU" informations |> Maybe.map .instructions |> Maybe.withDefault "I couldn't find the infos of the task : recR8areYkKRvQ6lU "
                                in
                                [ h1 [ class "flex flex-col w-full items-center justify-center" ] [ text "Instructions" ]
                                , p [ class "max-w-2xl text-xl text-center mb-8" ] [ View.fromMarkdown taskInfo ]
                                , View.button { message = Acceptability Acceptability.StartTraining, txt = "String", isDisabled = False }
                                ]

                            RemoteData.Failure reason ->
                                [ p [] [ text "I couldn't find the tasks infos. Please report this error." ] ]

                            RemoteData.Loading ->
                                []

                            RemoteData.NotAsked ->
                                []

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

                    Route.EmailSent ->
                        [ text "Un email a été envoyé. Veuillez cliquer sur le lien pour continuer l'expérience." ]

                    Route.PiloteInfos ->
                        [ form [ class "w-full max-w-lg" ]
                            [ div [ class "flex flex-wrap -mx-3 mb-6" ]
                                [ div [ class "w-full md:w-1/2 px-3 mb-6 md:mb-0" ]
                                    [ label [ for "is-student" ]
                                        [ text "Are you a student" ]
                                    , input [ id "is-student", type_ "radio" ] []
                                    ]
                                ]
                            ]
                        ]

                    Route.GeneralInfos ->
                        [ Pretest.GeneralInfos.view model.informations (\input -> Informations <| Pretest.GeneralInfos.UserUpdatedEmailField input) (\email -> Informations <| Pretest.GeneralInfos.UserClickedSendData email) ]

            Home ->
                [ div [ class "container flex flex-col items-center justify-center w-full max-w-2-xl" ]
                    [ h1 [] [ text "Lex Learn 👩\u{200D}🎓️" ]
                    , p
                        [ class "max-w-2xl text-xl text-center mb-8" ]
                        [ text "Lorem ipsum dolor sit amet, consectetur adipiscing elit, sed do eiusmod tempor incididunt ut labore et dolore magna aliqua.\n Sapien et ligula ullamcorper malesuada proin libero nunc consequat. Sed sed risus pretium quam vulputate dignissim. Aliquam sem fringilla ut morbi tincidunt augue interdum velit euismod. Ultrices tincidunt arcu non sodales neque."
                        ]
                    , a [ href "/pretest/informations" ] [ View.button { message = NoOp, txt = "Commencer les prétests", isDisabled = False } ]
                    ]
                ]

            NotFound ->
                View.notFound
    ]


type alias ShuffledSession1 =
    { meaning : List Meaning.Trial, spelling : List SpellingLvl1.Trial, cu1 : List CU1.Trial, presentation : List Presentation.Trial, infos_ : List ExperimentInfo.Task }


type alias ShuffledSession2 =
    { cu : List CU2.Trial, spelling : List Scrabble.Trial, translation : List Translation.Trial, infos : List ExperimentInfo.Task }


type alias ShuffledSession3 =
    { cu : List CU3.Trial, spelling : List Spelling3.Trial, synonym : List Synonym.Trial, infos : List ExperimentInfo.Task }


type Msg
    = ServerRespondedWithUserInfo (Result Http.Error User.AuthenticatedInfo)
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
    | UserDragsLetter DnDList.Msg
    | UserClickedLink Browser.UrlRequest
    | BrowserChangedUrl Url
    | NewTick Time.Posix
    | NoOp
      --
      --                                                          ##  ##  ### ### ###  ## ###
      --                                                          # # # # #    #  #   #    #
      --                                                          ##  ##  ##   #  ##   #   #
      --                                                          #   # # #    #  #     #  #
      --                                                          #   # # ###  #  ### ##   #
    | Acceptability Acceptability.Msg
    | UserPressedKey (Maybe Bool)
    | Informations Pretest.GeneralInfos.Msg
    | ServerRespondedWithSomePretestData (Para.Msg2 (List Acceptability.Trial) (List ExperimentInfo.Task))
    | ServerRespondedWithAllPretestData (List Acceptability.Trial) (List ExperimentInfo.Task)
    | ToNextStep Acceptability.Step
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
    | StartSession1 ShuffledSession1
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
    | StartSession2 ShuffledSession2
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
    | ServerRespondedWithSomeSession3Data (Para.Msg4 (List CU3.Trial) (List Spelling3.Trial) (List Synonym.Trial) (List ExperimentInfo.Task))
    | ServerRespondedWithAllSession3Data (List CU3.Trial) (List Spelling3.Trial) (List Synonym.Trial) (List ExperimentInfo.Task)
    | StartSession3 ShuffledSession3


pure model =
    ( model, Cmd.none )


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    let
        currentScrabbleState =
            case Logic.getState model.scrabbleTask of
                Just x ->
                    x

                _ ->
                    Scrabble.initState

        currentSpellingState =
            Logic.getState model.spellingLvl1
    in
    case msg of
        BrowserChangedUrl url ->
            ( { model | route = Route.fromUrl url }
            , Cmd.none
            )

        NoOp ->
            pure model

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

        ServerRespondedWithSomeError error ->
            ( { model | errorTracking = error :: model.errorTracking }, Cmd.none )

        Informations message ->
            case message of
                Pretest.GeneralInfos.UserClickedSendData email ->
                    let
                        body_ =
                            Json.Encode.object [ ( "Email", Json.Encode.string email ) ] |> Http.jsonBody

                        doRequest =
                            Http.post
                                { url =
                                    Data.buildQuery
                                        { app = Data.apps.spacing
                                        , base = "users"
                                        , view_ = "all"
                                        }
                                , body = body_
                                , expect = Http.expectWhatever (\result -> Informations (Pretest.GeneralInfos.UserCreated result))
                                }
                    in
                    ( model, doRequest )

                Pretest.GeneralInfos.UserUpdatedEmailField email ->
                    pure { model | informations = email }

                Pretest.GeneralInfos.UserCreated _ ->
                    ( { model | route = Route.Pretest Route.EmailSent }, Nav.pushUrl model.key "email-sent" )

        ServerRespondedWithAllSession1Data meaning spelling cu1 presentation infos_ ->
            let
                randomize =
                    Random.generate StartSession1 (Random.map5 ShuffledSession1 (shuffle meaning) (shuffle spelling) (shuffle cu1) (shuffle presentation) (Random.constant infos_))
            in
            ( { model
                | infos = RemoteData.Success (ExperimentInfo.toDict infos_)
              }
            , randomize
            )

        StartSession1 ({ infos_ } as tasks) ->
            ( { model
                | meaning = Meaning.start infos_ tasks.meaning
                , spellingLvl1 = SpellingLvl1.start infos_ tasks.spelling
                , cu1 = CU1.start infos_ tasks.cu1
                , presentation = Presentation.start infos_ tasks.presentation
                , session1 = Ready
              }
            , Cmd.none
            )

        ServerRespondedWithSomeSession2Data downloadMsg ->
            let
                ( updte, cmd ) =
                    case model.session2 of
                        Loading downloadState ->
                            Para.update4 downloadState downloadMsg |> Tuple.mapFirst Loading

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

                randomizeTrials =
                    Random.generate StartSession2 (Random.map4 ShuffledSession2 (shuffle cu) shuffleLetters (shuffle translation) (Random.constant infos_))
            in
            ( model
            , Cmd.batch [ randomizeTrials ]
              -- ( model, Random.generate (\shuffledData -> Shuffled (ServerRespondedWithScrabbleTrials (Result.Ok shuffledData))) shuffleLetters
            )

        StartSession2 { cu, spelling, translation, infos } ->
            ( { model
                | translationTask = Translation.start infos translation
                , cuLvl2 = CU2.start infos cu
                , scrabbleTask = Scrabble.start infos spelling
                , session2 = Ready
              }
            , Cmd.none
            )

        ServerRespondedWithSomeSession3Data downloadMsg ->
            let
                ( updte, cmd ) =
                    case model.session3 of
                        Loading downloadState ->
                            Para.update4 downloadState downloadMsg |> Tuple.mapFirst Loading

                        _ ->
                            ( model.session3, Cmd.none )
            in
            ( { model | session3 = updte }, cmd )

        ServerRespondedWithAllSession3Data cu spelling synonym infos ->
            let
                randomize =
                    Random.generate StartSession3 (Random.map4 ShuffledSession3 (shuffle cu) (shuffle spelling) (shuffle synonym) (Random.constant infos))
            in
            ( model
            , randomize
            )

        StartSession3 { cu, spelling, synonym, infos } ->
            ( { model
                | synonymTask = Synonym.start infos synonym
                , spelling3 = Spelling3.start infos spelling
                , cu3 = CU3.start infos cu
                , session3 = Ready
                , infos = RemoteData.Success (ExperimentInfo.toDict infos)
              }
            , Cmd.none
            )

        ServerRespondedWithUserInfo (Result.Ok _) ->
            ( model, Cmd.none )

        ServerRespondedWithUserInfo (Result.Err _) ->
            ( model, Cmd.none )

        ToNextStep newStep ->
            ( { model | acceptabilityTask = model.acceptabilityTask }, Cmd.none )

        NewTick float ->
            let
                prevState =
                    Logic.getState model.acceptabilityTask
            in
            case prevState of
                Just state ->
                    ( model, Cmd.none )

                Nothing ->
                    pure model

        Acceptability message ->
            let
                prevState =
                    Logic.getState model.acceptabilityTask

                toNextStep int step =
                    Delay.after int (Acceptability (Acceptability.NextStepCinematic step))

                getTrial =
                    Logic.getTrial model.acceptabilityTask
            in
            case ( prevState, getTrial ) of
                ( Just pState, Just trial ) ->
                    case message of
                        Acceptability.NextStepCinematic step ->
                            case step of
                                Acceptability.Listening ->
                                    ( { model | acceptabilityTask = Logic.update { pState | step = Acceptability.Listening } model.acceptabilityTask }
                                    , Delay.after 0 (PlaysoundInJS trial.audio.url)
                                    )

                                Acceptability.Answering ->
                                    ( { model | acceptabilityTask = Logic.update { pState | step = Acceptability.Answering } model.acceptabilityTask }, Cmd.none )

                                Acceptability.End ->
                                    ( { model | acceptabilityTask = Logic.update { pState | step = Acceptability.End } model.acceptabilityTask |> Logic.next pState }, toNextStep 500 Acceptability.Start )

                                Acceptability.Start ->
                                    ( { model | acceptabilityTask = Logic.update Acceptability.initState model.acceptabilityTask }, Delay.after 0 playBeep )

                        Acceptability.StartMain trials infos ->
                            ( { model | acceptabilityTask = Logic.startMain infos trials Acceptability.initState }, Cmd.none )

                        Acceptability.UserPressedButton maybeBool ->
                            ( model, Task.perform (\timestamp -> Acceptability (Acceptability.UserPressedButtonWithTimestamp maybeBool timestamp)) Time.now )

                        Acceptability.UserPressedButtonWithTimestamp maybeBool timestamp ->
                            case maybeBool of
                                Just bool ->
                                    ( { model
                                        | acceptabilityTask =
                                            Logic.update
                                                { pState
                                                    | step = Acceptability.End
                                                    , evaluation = bool
                                                    , userAnsweredAt = Just timestamp
                                                }
                                                model.acceptabilityTask
                                      }
                                    , toNextStep 0 Acceptability.End
                                    )

                                Nothing ->
                                    ( model, Cmd.none )

                        Acceptability.AudioEnded { endedAt, audioName } ->
                            let
                                cinematic =
                                    if audioName == beep then
                                        toNextStep 500 Acceptability.Listening

                                    else
                                        toNextStep 0 Acceptability.Answering
                            in
                            if audioName == beep then
                                ( { model | acceptabilityTask = Logic.update { pState | beepEndedAt = Just (Time.millisToPosix endedAt) } model.acceptabilityTask }, toNextStep 500 Acceptability.Listening )

                            else
                                ( { model | acceptabilityTask = Logic.update { pState | audioEndedAt = Just (Time.millisToPosix endedAt) } model.acceptabilityTask }
                                , toNextStep 0 Acceptability.Answering
                                )

                        Acceptability.StartTraining ->
                            ( model, Cmd.batch [ Delay.after 0 playBeep, Task.perform (always NoOp) (Browser.Dom.setViewport 0 400), Nav.pushUrl model.key "start" ] )

                        _ ->
                            Debug.todo ""

                ( _, _ ) ->
                    let
                        _ =
                            Debug.log "I have no previous state"
                    in
                    case message of
                        Acceptability.StartMain trials infos ->
                            ( { model | acceptabilityTask = Logic.startMain infos trials Acceptability.initState }, Delay.after 0 playBeep )

                        Acceptability.RuntimeShuffledTrials info trials ->
                            ( { model | acceptabilityTask = Acceptability.start info trials }, Cmd.none )

                        Acceptability.UserClickedSaveMsg ->
                            let
                                responseHandler =
                                    \records -> Acceptability (Acceptability.ServerRespondedWithLastRecords records)

                                taskId =
                                    "recR8areYkKRvQ6lU"
                            in
                            ( model, Logic.saveAcceptabilityData responseHandler model.user taskId model.acceptabilityTask )

                        _ ->
                            ( model, Cmd.none )

        ServerRespondedWithAllPretestData trials info ->
            let
                generateOrganizedTrials =
                    Random.List.shuffle trials
                        |> Random.andThen
                            (\shuffledTrials ->
                                let
                                    targets =
                                        List.filter (\datum -> datum.trialType == Acceptability.Target) shuffledTrials

                                    distractors =
                                        List.filter (\datum -> datum.trialType == Acceptability.Distractor) shuffledTrials
                                in
                                Random.constant (organizeAcceptabilityTrials targets distractors)
                                    |> Random.andThen
                                        (\organizedTrials_ ->
                                            case organizedTrials_ of
                                                Result.Err reason ->
                                                    let
                                                        _ =
                                                            Debug.log <| "Error while shuffling trials: " ++ reason
                                                    in
                                                    Random.constant []

                                                Result.Ok tr ->
                                                    Random.Extra.sequence (swapTargetWithOneDistractor tr)
                                        )
                            )
                        |> Random.generate (\shuffledTrials -> Acceptability (Acceptability.RuntimeShuffledTrials info (List.concat <| trainingTrials :: shuffledTrials)))

                trainingTrials =
                    List.filter (\datum -> datum.trialType == Acceptability.Training) trials

                swapTargetWithOneDistractor : List (List Acceptability.Trial) -> List (Random.Generator (List Acceptability.Trial))
                swapTargetWithOneDistractor tr =
                    List.map (\block -> Random.int 2 4 |> Random.andThen (\position -> Random.constant (List.Extra.swapAt 1 position block))) tr
            in
            ( { model | infos = RemoteData.Success (ExperimentInfo.toDict info) }, generateOrganizedTrials )

        -- beep
        --Delay.after 300 playBeep )
        ServerRespondedWithSomePretestData downloadMsg ->
            let
                ( updte, cmd ) =
                    case model.pilote of
                        Loading downloadState ->
                            Para.update2 downloadState downloadMsg |> Tuple.mapFirst Loading

                        _ ->
                            ( model.pilote, Cmd.none )
            in
            ( { model | pilote = updte }, cmd )

        UserPressedKey evaluation ->
            ( model
            , Task.perform (\time -> WithTime (UserPressedKey evaluation) time) Time.now
            )

        UserToggledInCloudWords word ->
            ( { model | cloudWords = CloudWords.toggle word model.cloudWords }, Cmd.none )

        UserDragsLetter dndmsg ->
            let
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
                            Logic.getState model.acceptabilityTask
                    in
                    case prevState of
                        Just pState ->
                            case evaluation of
                                Just x ->
                                    ( { model | acceptabilityTask = Logic.update { pState | evaluation = x } model.acceptabilityTask |> Logic.next Acceptability.initState }
                                    , Delay.after 500 playBeep
                                    )

                                Nothing ->
                                    ( model, Cmd.none )

                        _ ->
                            ( model, Cmd.none )

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
                                    , scrambledLetter = Scrabble.toItems nextTrial.writtenWord
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
                            ( { model | scrabbleTask = Logic.startMain infos trials { currentScrabbleState | userAnswer = x.writtenWord, scrambledLetter = Scrabble.toItems x.writtenWord } }, Cmd.none )

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

                Spelling3.UserClickedSaveData ->
                    let
                        responseHandler =
                            \records -> Spelling3 (Spelling3.ServerRespondedWithLastRecords records)
                    in
                    ( model, Logic.saveData responseHandler model.user taskId model.scrabbleTask )

                Spelling3.ServerRespondedWithLastRecords _ ->
                    ( model, Cmd.none )

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
                                |> Logic.toggle
                      }
                    , Cmd.none
                    )

                Synonym.UserChangedInput newChoice ->
                    ( { model
                        | synonymTask =
                            model.synonymTask
                                |> Logic.update { uid = "", userAnswer = newChoice }
                      }
                    , Cmd.none
                    )

                Synonym.UserClickedNextTrial ->
                    ( { model
                        | synonymTask =
                            model.synonymTask |> Logic.next Synonym.initState
                      }
                    , Cmd.none
                    )

                Synonym.SaveDataMsg ->
                    let
                        responseHandler =
                            \records -> Synonym (Synonym.ServerRespondedWithLastRecords records)
                    in
                    ( model, Logic.saveData responseHandler model.user Synonym.taskId model.translationTask )

                Synonym.UserClickedStartMainloop trials infos ->
                    ( { model | synonymTask = Logic.startMain infos trials Meaning.initState }, Cmd.none )

                Synonym.ServerRespondedWithLastRecords records ->
                    ( model, Cmd.none )

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


subscriptions : Model -> Sub Msg
subscriptions model =
    let
        acceptabilityState =
            Logic.getState model.acceptabilityTask

        listenToInput : Sub Msg
        listenToInput =
            case acceptabilityState of
                Just state ->
                    if state.step == Acceptability.Answering then
                        onKeyDown keyDecoder

                    else
                        Sub.none

                Nothing ->
                    Sub.none

        clock =
            case model.acceptabilityTask of
                Logic.Intr _ ->
                    Browser.Events.onAnimationFrame NewTick

                _ ->
                    Sub.none
    in
    Sub.batch
        [ system.subscriptions model.dnd
        , listenToInput

        --, clock
        , audioEnded (\{ endedAt, audioName } -> Acceptability (Acceptability.AudioEnded { endedAt = endedAt, audioName = audioName }))
        ]


keyDecoder : Json.Decode.Decoder Msg
keyDecoder =
    Json.Decode.map toEvaluation (Json.Decode.field "key" Json.Decode.string)



--toEvaluation : String -> Msg
--toEvaluation : String -> Msg


toEvaluation x =
    case x of
        "j" ->
            Acceptability (Acceptability.UserPressedButton (Just True))

        "f" ->
            Acceptability (Acceptability.UserPressedButton (Just False))

        _ ->
            Acceptability (Acceptability.UserPressedButton Nothing)


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
                    [ div [ class "flex flex-col items-center w-full" ] [ audioButton currentTrial.audioWord.url ]
                    , if not data.feedback then
                        div [ class "flex flex-col items-center w-full" ]
                            [ viewLetters data.state.scrambledLetter
                            , ghostView model.dnd
                                data.state.scrambledLetter
                            ]

                      else
                        div [] []
                    , View.genericSingleChoiceFeedback
                        { isVisible = data.feedback
                        , userAnswer = data.state.userAnswer
                        , target = currentTrial.target
                        , feedback_Correct =
                            ( data.infos.feedback_correct
                            , [ View.bold currentTrial.target ]
                            )
                        , feedback_Incorrect = ( data.infos.feedback_incorrect, [ View.bold currentTrial.target ] )
                        , button = View.navigationButton (Spelling2 UserClickedFeedbackButton) (Spelling2 (UserClickedNextTrial data.next)) data.feedback
                        }
                    ]

                Nothing ->
                    [ View.end data.infos.end (Spelling2 UserClickedSaveData) "context-understanding" ]

        Logic.Intr data ->
            case data.current of
                Just currentTrial ->
                    [ View.viewTraining data.infos.instructions
                        [ audioButton currentTrial.audioWord.url
                        , div [ class "col-start-2 col-span-4" ] [ viewLetters data.state.scrambledLetter ]
                        , ghostView model.dnd data.state.scrambledLetter
                        , View.genericSingleChoiceFeedback
                            { isVisible = data.feedback
                            , userAnswer = data.state.userAnswer
                            , target = currentTrial.target
                            , feedback_Correct =
                                ( data.infos.feedback_correct
                                , [ View.bold currentTrial.target ]
                                )
                            , feedback_Incorrect = ( data.infos.feedback_incorrect, [ View.bold currentTrial.target ] )
                            , button = View.navigationButton (Spelling2 UserClickedFeedbackButton) (Spelling2 (UserClickedNextTrial data.next)) data.feedback
                            }
                        ]
                    ]

                Nothing ->
                    [ View.introToMain (Spelling2 (UserClickedStartMainloop data.mainTrials data.infos)) ]

        Logic.Loading ->
            [ text "Loading..." ]

        Logic.Err reason ->
            [ text reason ]


itemView : DnDList.Model -> Int -> Scrabble.KeyedItem -> ( String, Html Msg )
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


ghostView : DnDList.Model -> List Scrabble.KeyedItem -> Html Msg
ghostView dnd items =
    let
        maybeDragItem : Maybe Scrabble.KeyedItem
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
-- SYSTEM


config : DnDList.Config Scrabble.KeyedItem
config =
    { beforeUpdate = \_ _ list -> list
    , movement = DnDList.Free
    , listen = DnDList.OnDrag
    , operation =
        DnDList.Swap
    }


system : DnDList.System Scrabble.KeyedItem Msg
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
                        , Html.Styled.Events.onClick <|
                            UserToggledInCloudWords word
                        ]
                        []
                    , span [ class "pl-4" ] [ text word ]
                    ]
            )
            (Dict.keys model.cloudWords)
        )


playBeep =
    PlaysoundInJS beep


beep =
    "https://dl.airtable.com/.attachments/9035dbbb358fe77bebaae1a3d0863e11/c3afa1ad/salamisound-4324105-signal-beep-once-as.mp3"


removesItemsHelp : List a -> List a -> List a -> List a
removesItemsHelp items ls acc =
    case ls of
        [] ->
            List.reverse acc

        x :: xs ->
            if List.member x items then
                removesItemsHelp items xs acc

            else
                removesItemsHelp items xs (x :: acc)


removesItems : List a -> List a -> List a
removesItems items ls =
    removesItemsHelp items ls []


organizeAcceptabilityTrialsHelper : List Acceptability.Trial -> List Acceptability.Trial -> List (List Acceptability.Trial) -> Result.Result String (List (List Acceptability.Trial))
organizeAcceptabilityTrialsHelper targets distractors output =
    -- Acceptability trials must be organized in sequence of blocks containing exactly one target and 3 distractors belonging to 3 different sentence type.
    -- After shuffling all the trials, this function is used create the proper sequence.
    -- Because the target can't be at the first position of a sequence, we have to swap the position of the target with one of the following distractors. TODO
    let
        nextNewSentenceType buff dis =
            List.member dis.sentenceType (whichSentenceTypes buff) |> not

        buildBlock t =
            List.head t
                |> Result.fromMaybe "I couldn't find the first target"
                |> Result.andThen
                    (\foundTarget ->
                        List.Extra.find (nextNewSentenceType []) distractors
                            |> Result.fromMaybe "I couldn't find the first distractor"
                            |> Result.andThen
                                (\distractorFound ->
                                    List.Extra.find (nextNewSentenceType [ distractorFound ])
                                        (removesItems [ distractorFound ] distractors)
                                        |> Result.fromMaybe "I couldn't find the second distractor"
                                        |> Result.andThen
                                            (\secondDistractorFound ->
                                                List.Extra.find (nextNewSentenceType [ distractorFound, secondDistractorFound ]) (removesItems [ distractorFound, secondDistractorFound ] distractors)
                                                    |> Result.fromMaybe "I couldn't find the thirdDistractor"
                                                    |> Result.andThen
                                                        (\thirdDistractorFound ->
                                                            Result.Ok
                                                                { target = foundTarget
                                                                , firstDistractor = distractorFound
                                                                , secondDistractor = secondDistractorFound
                                                                , thirdDistractor = thirdDistractorFound
                                                                , remainingDistractors = removesItems [ distractorFound, secondDistractorFound, thirdDistractorFound ] distractors
                                                                }
                                                        )
                                            )
                                )
                    )
    in
    case targets of
        [] ->
            Result.Ok output

        x :: xs ->
            case buildBlock targets of
                Result.Err reason ->
                    Result.Err reason

                Result.Ok { target, firstDistractor, secondDistractor, thirdDistractor, remainingDistractors } ->
                    organizeAcceptabilityTrialsHelper xs remainingDistractors ([ [ target, firstDistractor, secondDistractor, thirdDistractor ] ] ++ output)


type alias Block =
    { target : Acceptability.Trial
    , firstDistractor : Acceptability.Trial
    , secondDistractor : Acceptability.Trial
    , thirdDistractor : Acceptability.Trial
    , remainingDistractors : List Acceptability.Trial
    }


areMultipleOf4 targets distractors =
    (List.length (targets ++ distractors) |> modBy 4) == 0


isNextSentence : { a | sentenceType : Acceptability.SentenceType } -> List { b | sentenceType : Acceptability.SentenceType } -> Bool
isNextSentence dis blockBuffer =
    List.member dis.sentenceType (whichSentenceTypes blockBuffer) |> not


whichSentenceTypes : List { a | sentenceType : Acceptability.SentenceType } -> List Acceptability.SentenceType
whichSentenceTypes sentences =
    List.map .sentenceType sentences


organizeAcceptabilityTrials : List Acceptability.Trial -> List Acceptability.Trial -> Result.Result String (List (List Acceptability.Trial))
organizeAcceptabilityTrials targets distractors =
    organizeAcceptabilityTrialsHelper targets distractors []


jumpToBottom : String -> Cmd Msg
jumpToBottom id =
    Browser.Dom.getViewportOf id
        |> Task.andThen (\info -> Browser.Dom.setViewportOf id 0 info.scene.height)
        |> Task.attempt (always NoOp)
