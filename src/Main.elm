port module Main exposing
    ( isNextSentence
    , main
    , nextNewSentenceType
    , organizeAcceptabilityTrials
    , removesItems
    , toEvaluation
    )

import Browser
import Browser.Events exposing (onKeyDown)
import Browser.Navigation as Nav exposing (pushUrl)
import Data
import Delay
import Dict
import DnDList
import DnDList.Groups exposing (Model)
import ExperimentInfo exposing (Session(..), Task)
import Html.Styled exposing (..)
import Html.Styled.Attributes exposing (class, href, id, type_)
import Html.Styled.Events
import Http
import Json.Decode
import List.Extra
import Logic
import Ports
import Postest.CloudWords as CloudWords
import Postest.YN as YN
import Pretest.Acceptability as Acceptability
import Pretest.GeneralInfos
import Pretest.Pretest as Pretest
import Pretest.SPR as SPR
import Pretest.SentenceCompletion as SentenceCompletion
import Pretest.VKS as VKS
import Random
import Random.Extra
import Random.List exposing (shuffle)
import RemoteData exposing (RemoteData)
import Result exposing (Result)
import Route exposing (Route(..), Session1Task(..), Session2Task(..))
import Session exposing (Session(..))
import Session1.ContextUnderstanding as CU1
import Session1.Meaning as Meaning
import Session1.Presentation as Presentation
import Session1.Session as Session1
import Session1.Spelling as SpellingLvl1
import Session1.Top
import Session2.CU2 as CU2
import Session2.Session as Session2
import Session2.Spelling as Scrabble
import Session2.Translation as Translation
import Session3.CU3 as CU3
import Session3.Session as Session3
import Session3.Spelling3 as Spelling3
import Session3.Synonym as Synonym
import Task
import Task.Parallel as Para
import Time
import Url exposing (Url)
import Url.Builder
import User
import View exposing (navOut)


type alias Flags =
    {}


port audioEnded : ({ eventType : String, name : String, timestamp : Int } -> msg) -> Sub msg



-- MODEL


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
    , yn : Logic.Task YN.Trial YN.State
    , acceptabilityTask : Logic.Task Acceptability.Trial Acceptability.State
    , packetsSended : Int
    , informations : Pretest.GeneralInfos.Model
    , pilote : Pilote
    , spr : SPR.SPR
    , pretest : Pretest.Pretest
    , sentenceCompletion : SentenceCompletion.SentenceCompletion
    , vks : Logic.Task VKS.Trial VKS.State

    --
    --                                                                  ## ###  ##  ## ###  #  ###      #
    --                                                                 #   #   #   #    #  # # # #     ##
    --                                                                  #  ##   #   #   #  # # # #      #
    --                                                                   # #     #   #  #  # # # #      #
    --                                                                 ##  ### ##  ##  ###  #  # #     ###
    , session1 : Session1.Session1
    , meaning : Logic.Task Meaning.Trial Meaning.State
    , spellingLvl1 : Logic.Task SpellingLvl1.Trial SpellingLvl1.State

    --cu1 stands for context-understanding-1
    , cu1 : Logic.Task CU1.Trial CU1.State
    , presentation : Logic.Task Presentation.Trial Presentation.State
    , endAcceptabilityDuration : Int

    --                                                                  .dP"Y8 888888 .dP"Y8 .dP"Y8 88  dP"Yb  88b 88     oP"Yb.
    --                                                                  `Ybo." 88__   `Ybo." `Ybo." 88 dP   Yb 88Yb88     "' dP'
    --                                                                  o.`Y8b 88""   o.`Y8b o.`Y8b 88 Yb   dP 88 Y88       dP'
    --                                                                  8bodP' 888888 8bodP' 8bodP' 88  YbodP  88  Y8     .d8888
    , translationTask : Logic.Task Translation.Trial Translation.State
    , scrabbleTask : Logic.Task Scrabble.Trial Scrabble.State
    , cuLvl2 : Logic.Task CU2.Trial CU2.State
    , session2 : Session2.Session2

    --                                                               .dP"Y8 888888 .dP"Y8 .dP"Y8 88  dP"Yb  88b 88     88888
    --                                                               `Ybo." 88__   `Ybo." `Ybo." 88 dP   Yb 88Yb88       .dP
    --                                                               o.`Y8b 88""   o.`Y8b o.`Y8b 88 Yb   dP 88 Y88     o `Yb
    --                                                               8bodP' 888888 8bodP' 8bodP' 88  YbodP  88  Y8     YbodP
    , spelling3 : Logic.Task Spelling3.Trial Spelling3.State
    , cu3 : Logic.Task CU3.Trial CU3.State
    , synonymTask : Logic.Task Synonym.Trial Synonym.State
    , session3 : Session3.Session3

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
    }


init : Flags -> Url -> Nav.Key -> ( Model, Cmd Msg )
init _ url key =
    let
        route =
            Route.fromUrl url

        ( loadingStateSession1, fetchSession1 ) =
            Session1.getAll

        ( loadingStateSession2, fetchSession2 ) =
            Session2.getAll

        ( loadingStateSession3, fetchSession3 ) =
            Session3.attempt

        defaultInit =
            { key = key
            , route = route
            , dnd = Scrabble.system.model

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

            -- PILOTE
            , acceptabilityTask = Logic.NotStarted
            , packetsSended = 0
            , endAcceptabilityDuration = 6000
            , pilote = NotAsked

            -- PRETEST
            , yn = Logic.Loading
            , informations = ""
            , spr = Logic.NotStarted
            , pretest = Tuple.first Pretest.attempt
            , sentenceCompletion = Logic.NotStarted
            , vks = Logic.NotStarted

            -- POSTEST
            , cloudWords = Dict.fromList CloudWords.words

            -- SHARED
            , user = url.query
            , optionsOrder = [ 0, 1, 2, 3 ]
            , infos = RemoteData.Loading
            , errorTracking = []
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
                , session1 = Loading loadingStateSession1
              }
            , Cmd.map Session1 fetchSession1
            )

        Route.Home ->
            ( { defaultInit
                | -- SESSION 1
                  meaning = Logic.Loading
                , spellingLvl1 = Logic.Loading
                , cu1 = Logic.Loading
                , presentation = Logic.Loading
                , user = Nothing
                , session1 = Loading loadingStateSession1
              }
            , Cmd.map Session1 fetchSession1
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
            , Cmd.map Session2 fetchSession2
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
            , Cmd.map Session3 fetchSession3
            )

        Route.Pretest _ ->
            ( { defaultInit
                | spr = Logic.Loading
                , sentenceCompletion = Logic.Loading
              }
            , Cmd.map Pretest (Tuple.second Pretest.attempt)
            )

        Route.Pilote userid _ ->
            ( { defaultInit
                | acceptabilityTask = Logic.Loading
                , pilote = Session.NotAsked
                , user = Just userid
              }
            , Cmd.none
              --fetchPilote
            )

        Route.Posttest _ ->
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
                            _ =
                                Dict.get "Meaning" infos
                        in
                        [ Html.Styled.map Meaning <|
                            Meaning.view
                                { task = model.meaning
                                , optionsOrder = model.optionsOrder
                                }
                        ]

                    Route.Presentation ->
                        [ Html.Styled.map Presentation (Presentation.view model.presentation)
                        ]

                    SpellingLevel1 ->
                        [ Html.Styled.map Spelling1
                            (SpellingLvl1.view model.spellingLvl1
                                model.optionsOrder
                            )
                        ]

                    Route.CU1 ->
                        [ Html.Styled.map CU1
                            (CU1.view
                                { task = model.cu1
                                , optionsOrder = model.optionsOrder
                                }
                            )
                        ]

            Route.AuthenticatedSession2 _ task ->
                case task of
                    CU ->
                        [ Html.Styled.map CU2
                            (CU2.view model.cuLvl2
                                model.optionsOrder
                            )
                        ]

                    Route.Translation ->
                        [ Html.Styled.map Translation
                            (Translation.view
                                { task = model.translationTask
                                , optionsOrder = model.optionsOrder
                                }
                            )
                        ]

                    Route.Spelling ->
                        [ Html.Styled.map Spelling2 (Scrabble.viewScrabbleTask model) ]

            Route.AuthenticatedSession3 _ task ->
                case task of
                    Route.CU3 ->
                        let
                            _ =
                                Dict.get "Context Understanding level 3" infos
                        in
                        [ Html.Styled.map CU3 <| CU3.view model.cu3
                        ]

                    Route.Synonym ->
                        List.map (Html.Styled.map Synonym) <| Synonym.viewTask model.synonymTask

                    Route.Spelling3 ->
                        let
                            _ =
                                Dict.get "Spelling level 3" infos
                        in
                        [ Html.Styled.map Spelling3 <| Spelling3.view model.spelling3
                        ]

            Route.Posttest task ->
                case task of
                    Route.CloudWords ->
                        [ viewCloud model ]

            Route.Pilote _ task ->
                case task of
                    Route.AcceptabilityEnd ->
                        [ p [] [ text "Thanks again for your time and your help with this project. \nContact shona.whyte@univ-cotedazur.fr if you have any questions or comments." ] ]

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
                                [ div [ class "container flex flex-col items-center justify-center w-full max-w-2-xl" ]
                                    [ h1 [ class "" ] [ text "Instructions" ]
                                    , p [ class "max-w-2xl text-xl text-center mb-8" ] [ View.fromMarkdown taskInfo ]
                                    , View.button { message = Acceptability Acceptability.StartTraining, txt = "start", isDisabled = False }
                                    ]
                                ]

                            RemoteData.Failure _ ->
                                [ p [] [ text "I couldn't find the tasks infos. Please report this error." ] ]

                            RemoteData.Loading ->
                                [ text "Loading..." ]

                            RemoteData.NotAsked ->
                                [ text "Info not asked" ]

            Route.Pretest task ->
                case task of
                    Route.YN ->
                        let
                            _ =
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

                    Route.SPR ->
                        List.map (Html.Styled.map SPR) (SPR.view model.spr)

                    Route.SentenceCompletion ->
                        List.map (Html.Styled.map SentenceCompletion) (SentenceCompletion.view model.sentenceCompletion)

                    Route.GeneralInfos ->
                        []

                    Route.VKS ->
                        List.map (Html.Styled.map VKS) (VKS.view model.vks)

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
    | UserClickedLink Browser.UrlRequest
    | BrowserChangedUrl Url
    | NoOp
      --
      --                                                          ##  ##  ### ### ###  ## ###
      --                                                          # # # # #    #  #   #    #
      --                                                          ##  ##  ##   #  ##   #   #
      --                                                          #   # # #    #  #     #  #
      --                                                          #   # # ###  #  ### ##   #
    | Acceptability Acceptability.Msg
    | Pretest Pretest.Msg
    | SentenceCompletion SentenceCompletion.Msg
    | ServerRespondedWithAllPretestData (List Acceptability.Trial) (List ExperimentInfo.Task)
    | ServerRespondedWithSomePretestData (Para.Msg2 (List Acceptability.Trial) (List ExperimentInfo.Task))
    | SPR SPR.Msg
    | ToNextStep Acceptability.Step
    | UserPressedKey (Maybe Bool)
    | VKS VKS.Msg
      --
      --                                                          ## ###  ##  ## ###  #  ###      #
      --                                                         #   #   #   #    #  # # # #     ##
      --                                                          #  ##   #   #   #  # # # #      #
      --                                                           # #     #   #  #  # # # #      #
      --                                                         ##  ### ##  ##  ###  #  # #     ###
    | CU1 CU1.Msg
    | Presentation Presentation.Msg
    | Meaning Meaning.Msg
    | Spelling1 SpellingLvl1.Msg
    | Session1 Session1.Msg
      --
      --                                                          ## ###  ##  ## ###  #  ###     ###
      --                                                         #   #   #   #    #  # # # #       #
      --                                                          #  ##   #   #   #  # # # #     ###
      --                                                           # #     #   #  #  # # # #     #
      --                                                         ##  ### ##  ##  ###  #  # #     ###
    | CU2 CU2.CU2Msg
    | Spelling2 Scrabble.Msg
    | Translation Translation.Msg
    | Session2 Session2.Msg
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
    | Session3 Session3.Msg


pure model =
    ( model, Cmd.none )


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
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

        ServerRespondedWithUserInfo (Result.Ok _) ->
            ( model, Cmd.none )

        ServerRespondedWithUserInfo (Result.Err _) ->
            ( model, Cmd.none )

        ToNextStep _ ->
            ( { model | acceptabilityTask = model.acceptabilityTask }, Cmd.none )

        SPR submsg ->
            let
                ( newModel, newCmd ) =
                    SPR.update submsg model
            in
            ( newModel, Cmd.map SPR newCmd )

        Pretest submsg ->
            let
                ( newModel, newCmd ) =
                    Pretest.update submsg model
            in
            ( newModel, Cmd.map Pretest newCmd )

        Session1 submsg ->
            let
                ( newModel, newCmd ) =
                    Session1.update submsg model
            in
            ( newModel, Cmd.map Session1 newCmd )

        Session2 submsg ->
            let
                ( newModel, newCmd ) =
                    Session2.update submsg model
            in
            ( newModel, Cmd.map Session2 newCmd )

        Session3 submsg ->
            let
                ( newModel, newCmd ) =
                    Session3.update submsg model
            in
            ( newModel, Cmd.map Session3 newCmd )

        SentenceCompletion submsg ->
            let
                ( newModel, newCmd ) =
                    SentenceCompletion.update submsg model
            in
            ( newModel, Cmd.map SentenceCompletion newCmd )

        VKS submsg ->
            let
                ( newModel, newCmd ) =
                    VKS.update submsg model
            in
            ( newModel, Cmd.map VKS newCmd )

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
                                    , Delay.after 500 (PlaysoundInJS trial.audio.url)
                                    )

                                Acceptability.Answering ->
                                    ( { model | acceptabilityTask = Logic.update { pState | step = Acceptability.Answering } model.acceptabilityTask }, Delay.after trial.timeout (Acceptability (Acceptability.UserPressedButton Nothing)) )

                                Acceptability.End ->
                                    ( { model | acceptabilityTask = Logic.update { pState | step = Acceptability.End } model.acceptabilityTask |> Logic.next pState }
                                    , toNextStep 0 Acceptability.Start
                                    )

                                Acceptability.Start ->
                                    ( { model | acceptabilityTask = Logic.update Acceptability.newLoop model.acceptabilityTask }, Delay.after 0 (PlaysoundInJS beep) )

                                _ ->
                                    ( model, Cmd.none )

                        Acceptability.StartMain _ _ ->
                            ( { model | acceptabilityTask = Logic.startMain model.acceptabilityTask Acceptability.initState }, Cmd.none )

                        Acceptability.UserPressedButton maybeBool ->
                            let
                                forward =
                                    if pState.step == Acceptability.Answering then
                                        Task.perform (\timestamp -> Acceptability (Acceptability.UserPressedButtonWithTimestamp maybeBool timestamp)) Time.now

                                    else
                                        Cmd.none
                            in
                            ( model, forward )

                        Acceptability.UserPressedButtonWithTimestamp maybeBool timestamp ->
                            ( { model
                                | acceptabilityTask =
                                    Logic.update
                                        { pState
                                            | step = Acceptability.End
                                            , evaluation = Acceptability.maybeBoolToEvaluation maybeBool
                                            , userAnsweredAt = Just timestamp
                                        }
                                        model.acceptabilityTask
                              }
                            , toNextStep model.endAcceptabilityDuration Acceptability.End
                            )

                        Acceptability.AudioEnded ( name, timestamp ) ->
                            if name == beep then
                                ( { model | acceptabilityTask = Logic.update { pState | beepEndedAt = Just timestamp } model.acceptabilityTask }, Cmd.none )

                            else
                                ( { model | acceptabilityTask = Logic.update { pState | audioEndedAt = Just timestamp } model.acceptabilityTask }
                                , toNextStep 0 Acceptability.Answering
                                )

                        Acceptability.AudioStarted ( name, timestamp ) ->
                            if name == beep then
                                ( { model | acceptabilityTask = Logic.update { pState | beepStartedAt = Just timestamp } model.acceptabilityTask }, toNextStep 0 Acceptability.Listening )

                            else
                                ( { model | acceptabilityTask = Logic.update { pState | audioStartedAt = Just timestamp } model.acceptabilityTask }
                                , Cmd.none
                                )

                        Acceptability.StartTraining ->
                            ( model, Cmd.batch [ Nav.pushUrl model.key "start" ] )

                        _ ->
                            ( model, Cmd.none )

                ( _, _ ) ->
                    case message of
                        Acceptability.StartMain _ _ ->
                            ( { model | acceptabilityTask = Logic.startMain model.acceptabilityTask Acceptability.initState, endAcceptabilityDuration = 500 }, toNextStep 0 Acceptability.Init )

                        Acceptability.RuntimeShuffledTrials info trials ->
                            case trials of
                                Result.Ok shuffledTrials ->
                                    if List.filter (\block -> List.length block < 4) shuffledTrials == [ [] ] then
                                        ( { model | acceptabilityTask = Acceptability.start info (List.concat shuffledTrials) }, Cmd.none )

                                    else
                                        ( model, Delay.after 0 (ServerRespondedWithAllPretestData (List.concat shuffledTrials) info) )

                                Result.Err ( _, _ ) ->
                                    ( { model | acceptabilityTask = Logic.Err "Error whhen I tried to shuffle trials" }, Cmd.none )

                        Acceptability.UserClickedSaveMsg ->
                            let
                                responseHandler =
                                    \records -> Acceptability (Acceptability.ServerRespondedWithLastRecords records)
                            in
                            ( { model | acceptabilityTask = Logic.Loading }, Acceptability.saveAcceptabilityData responseHandler model.user model.acceptabilityTask )

                        Acceptability.ServerRespondedWithLastRecords (Result.Ok _) ->
                            ( { model | acceptabilityTask = Logic.Loading }
                            , pushUrl model.key "end"
                            )

                        Acceptability.ServerRespondedWithLastRecords (Result.Err reason) ->
                            ( { model | acceptabilityTask = Logic.Err <| Data.buildErrorMessage reason ++ "Please report this error message to yacourt@unice.fr with a nice screenshot!" }, Cmd.none )

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
                                                    Random.constant (Result.Err reason)

                                                Result.Ok tr ->
                                                    Random.Extra.sequence (swapTargetWithOneDistractor tr)
                                                        |> Random.andThen
                                                            (\swapedTargets ->
                                                                Random.constant
                                                                    (--if notLeakingBlock then
                                                                     Result.Ok
                                                                        (trainingTrials :: swapedTargets)
                                                                     --else
                                                                     -- Result.Err Acceptability.FirstDistractorMissing
                                                                    )
                                                            )
                                        )
                            )
                        |> Random.generate
                            (\st ->
                                Acceptability
                                    (Acceptability.RuntimeShuffledTrials info st)
                            )

                _ =
                    List.Extra.gatherEqualsBy .sentenceType distractors_

                _ =
                    List.filter (\datum -> datum.trialType == Acceptability.Target) trials

                distractors_ =
                    List.filter (\datum -> datum.trialType == Acceptability.Distractor) trials

                trainingTrials =
                    List.filter (\datum -> datum.trialType == Acceptability.Training) trials

                swapTargetWithOneDistractor : List (List Acceptability.Trial) -> List (Random.Generator (List Acceptability.Trial))
                swapTargetWithOneDistractor tr =
                    List.map
                        (\block ->
                            Random.int 1 (List.length block - 1)
                                |> Random.andThen
                                    (\position ->
                                        let
                                            _ =
                                                List.Extra.swapAt 0 position block
                                        in
                                        Random.constant (List.Extra.swapAt 0 position block)
                                    )
                        )
                        tr
            in
            ( { model | infos = RemoteData.Success (ExperimentInfo.toDict info) }, generateOrganizedTrials )

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

        WithTime message _ ->
            case message of
                UserPressedKey evaluation ->
                    let
                        prevState =
                            Logic.getState model.acceptabilityTask
                    in
                    case prevState of
                        Just pState ->
                            ( { model | acceptabilityTask = Logic.update { pState | evaluation = Acceptability.maybeBoolToEvaluation evaluation } model.acceptabilityTask |> Logic.next Acceptability.initState }
                            , Delay.after 500 playBeep
                            )

                        _ ->
                            ( model, Cmd.none )

                _ ->
                    ( model, Cmd.none )

        RuntimeShuffledOptionsOrder newOrder ->
            ( { model | optionsOrder = newOrder }, Cmd.none )

        PlaysoundInJS url ->
            ( model, Ports.playAudio url )

        Spelling1 message ->
            let
                ( newModel, newCmd ) =
                    SpellingLvl1.update message model
            in
            ( newModel, Cmd.map Spelling1 newCmd )

        CU2 message ->
            let
                ( newModel, newCmd ) =
                    CU2.update message model
            in
            ( newModel, Cmd.map CU2 newCmd )

        Spelling2 message ->
            let
                ( newModel, newCmd ) =
                    Scrabble.update message model
            in
            ( newModel, Cmd.map Spelling2 newCmd )

        CU1 submsg ->
            let
                ( newModel, newCmd ) =
                    CU1.update submsg model
            in
            ( newModel, Cmd.map CU1 newCmd )

        CU3 message ->
            let
                ( newModel, newCmd ) =
                    CU3.update message model
            in
            ( newModel, Cmd.map CU3 newCmd )

        Spelling3 message ->
            let
                ( newModel, newCmd ) =
                    Spelling3.update message model
            in
            ( newModel, Cmd.map Spelling3 newCmd )

        YN message ->
            let
                _ =
                    "rechYdq4MyLcb2nRG"
            in
            case message of
                YN.UserClickedNextTrial ->
                    ( { model | yn = Logic.next CU1.initState model.yn }, Cmd.none )

                YN.UserClickedToggleFeedback ->
                    ( { model | yn = Logic.toggle model.yn }, Cmd.none )

                YN.UserClickedStartIntro _ ->
                    ( model, Cmd.none )

                YN.UserClickedStartMain _ _ ->
                    ( { model | yn = Logic.startMain model.yn YN.initState }, Cmd.none )

                YN.UserChangedInput new ->
                    ( { model | yn = Logic.update { uid = "", userAnswer = new } model.yn }, Cmd.none )

        Presentation message ->
            let
                ( subModel, subCmd ) =
                    Presentation.update message model
            in
            ( subModel, Cmd.map Presentation subCmd )

        Synonym message ->
            let
                ( newModel, newCmd ) =
                    Synonym.update message model
            in
            ( newModel, Cmd.map Synonym newCmd )

        Meaning submsg ->
            let
                ( subModel, subCmd ) =
                    Meaning.update submsg model
            in
            ( subModel, Cmd.map Meaning subCmd )

        Translation submsg ->
            let
                ( subModel, subCmd ) =
                    Translation.update submsg model
            in
            ( subModel, Cmd.map Translation subCmd )


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

                    else if state.step == Acceptability.Init then
                        onKeyDown decodeSpace

                    else
                        Sub.none

                Nothing ->
                    Sub.none
    in
    Sub.batch
        [ Sub.map Spelling2 (Scrabble.subscriptions model)
        , listenToInput
        , audioEnded toAcceptabilityMessage
        , Sub.map SPR (SPR.subscriptions model.spr)
        ]



--toAcceptabilityMessage : Json.Decode.Decoder InboundAudioInfos -> Json.Decode.Decoder Msg


toAcceptabilityMessage { eventType, name, timestamp } =
    case eventType of
        "SoundStarted" ->
            Acceptability (Acceptability.AudioStarted ( name, Time.millisToPosix timestamp ))

        "SoundEnded" ->
            Acceptability (Acceptability.AudioEnded ( name, Time.millisToPosix timestamp ))

        _ ->
            NoOp


type JsAudioEvent
    = AudioStarted String Time.Posix
    | AudioEnded String Time.Posix


type alias InboundAudioInfos =
    { eventType : String
    , name : String
    , timestamp : Time.Posix
    }


type EventType
    = SoundStarted
    | SoundEnded


keyDecoder : Json.Decode.Decoder Msg
keyDecoder =
    Json.Decode.map toEvaluation (Json.Decode.field "key" Json.Decode.string)


decodeSpace : Json.Decode.Decoder Msg
decodeSpace =
    Json.Decode.map
        (\k ->
            case k of
                " " ->
                    Acceptability (Acceptability.NextStepCinematic Acceptability.Start)

                _ ->
                    NoOp
        )
        (Json.Decode.field "key" Json.Decode.string)



--toEvaluation : String -> Msg
--toEvaluation : String -> Msg


toEvaluation : String -> Msg
toEvaluation x =
    case x of
        "j" ->
            Acceptability (Acceptability.UserPressedButton (Just True))

        "f" ->
            Acceptability (Acceptability.UserPressedButton (Just False))

        _ ->
            Acceptability (Acceptability.UserPressedButton Nothing)


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


viewCloud : Model -> Html Msg
viewCloud model =
    div [ class "grid grid-flow-col grid-rows-4 auto-cols-max gap-4 " ]
        (List.map
            (\word ->
                let
                    _ =
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
    "https://dl.airtable.com/.attachments/b000c72585c5f5145828b1cf3916c38d/88d9c821/beep.mp3"


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


organizeAcceptabilityTrialsHelper : List Acceptability.Trial -> List Acceptability.Trial -> List (List Acceptability.Trial) -> Result.Result ( Acceptability.ErrorBlock, List Acceptability.Trial ) (List (List Acceptability.Trial))
organizeAcceptabilityTrialsHelper targets distractors output =
    -- Acceptability trials must be organized in sequence of blocks containing exactly one target and 3 distractors belonging to 3 different sentence type.
    -- After shuffling all the trials, this function is used create the proper sequence.
    -- Because the target can't be at the first position of a sequence, we have to swap the position of the target with one of the following distractors. TODO
    -- En gros, ça va marcher tant qu'il y a le bon nombre d'items mais s'il devait y avoir un déséquilibre, cela créera une recursion infinie.
    -- C'est le pire code de l'enfer, désolé si quelqu'un d'autre que moi voit ce massacre.
    let
        nextGrammaticalSentence buff dis =
            dis.isGrammatical && not (List.member dis.sentenceType (getSentenceTypes buff))

        --not (List.member dis.sentenceType (getSentenceTypes buff))
        --&& not (List.member dis.sentenceType (whichSentenceTypes buff))
        nextUngrammaticalSentence buff dis =
            not dis.isGrammatical && not (List.member dis.sentenceType (getSentenceTypes buff))

        --List.member dis.sentenceType (getSentenceTypes buff) |> not
        findFirstGrammaticalDistractor =
            List.Extra.find (nextGrammaticalSentence []) distractors

        findSecondGrammaticalDistractor firstDistractor =
            List.Extra.find (nextGrammaticalSentence firstDistractor) (removesItems firstDistractor distractors)

        findThirdGrammaticalDistractor firstDistractor secondDistractor =
            List.Extra.find (nextGrammaticalSentence [ firstDistractor, secondDistractor ]) (removesItems [ firstDistractor, secondDistractor ] distractors)

        firstUnGrammaticalDistractor =
            List.Extra.find (nextUngrammaticalSentence []) distractors

        findSecondUnGrammaticalDistractor firstDistractor =
            removesItems [ firstDistractor ] distractors
                |> List.Extra.find (nextUngrammaticalSentence [ firstDistractor ])

        findThirdUnGrammaticalDistractor firstDistractor secondDistractor =
            removesItems [ firstDistractor, secondDistractor ] distractors
                |> List.Extra.find (nextUngrammaticalSentence [ firstDistractor, secondDistractor ])

        buildBlock target =
            if target.isGrammatical then
                firstUnGrammaticalDistractor
                    |> Result.fromMaybe ( Acceptability.FirstDistractorMissing False, [ target ] )
                    |> Result.andThen
                        (\distractorFound ->
                            findSecondGrammaticalDistractor [ distractorFound ]
                                |> Result.fromMaybe
                                    ( Acceptability.SecondDistractorMissing True
                                    , [ target, distractorFound ]
                                    )
                                |> Result.andThen
                                    (\secondDistractorFound ->
                                        findThirdUnGrammaticalDistractor distractorFound secondDistractorFound
                                            |> Result.fromMaybe
                                                ( Acceptability.ThirdDistractorMissing False
                                                , [ target, distractorFound, secondDistractorFound ]
                                                )
                                            |> Result.andThen
                                                (\thirdDistractorFound ->
                                                    Result.Ok
                                                        { target = target
                                                        , firstDistractor = distractorFound
                                                        , secondDistractor = secondDistractorFound
                                                        , thirdDistractor = thirdDistractorFound
                                                        , remainingDistractors = removesItems [ distractorFound, secondDistractorFound, thirdDistractorFound ] distractors
                                                        }
                                                )
                                    )
                        )

            else
                findFirstGrammaticalDistractor
                    |> Result.fromMaybe ( Acceptability.FirstDistractorMissing True, [ target ] )
                    |> Result.andThen
                        (\distractorFound ->
                            findSecondUnGrammaticalDistractor distractorFound
                                |> Result.fromMaybe ( Acceptability.SecondDistractorMissing False, [ target, distractorFound ] )
                                |> Result.andThen
                                    (\secondDistractorFound ->
                                        findThirdGrammaticalDistractor distractorFound secondDistractorFound
                                            |> Result.fromMaybe ( Acceptability.ThirdDistractorMissing True, [ target, distractorFound, secondDistractorFound ] )
                                            |> Result.andThen
                                                (\thirdDistractorFound ->
                                                    Result.Ok
                                                        { target = target
                                                        , firstDistractor = distractorFound
                                                        , secondDistractor = secondDistractorFound
                                                        , thirdDistractor = thirdDistractorFound
                                                        , remainingDistractors = removesItems [ distractorFound, secondDistractorFound, thirdDistractorFound ] distractors
                                                        }
                                                )
                                    )
                        )

        _ =
            List.Extra.gatherEqualsBy .sentenceType distractors
    in
    case targets of
        [] ->
            let
                _ =
                    List.filter (\block -> List.length block < 4) (output ++ [ distractors ])
            in
            Result.Ok (output ++ [ distractors ])

        x :: xs ->
            case buildBlock x of
                Result.Err ( _, blockSoFar ) ->
                    organizeAcceptabilityTrialsHelper xs (removesItems blockSoFar distractors) (blockSoFar :: output)

                Result.Ok { target, firstDistractor, secondDistractor, thirdDistractor, remainingDistractors } ->
                    let
                        block =
                            [ target, firstDistractor, secondDistractor, thirdDistractor ]
                    in
                    organizeAcceptabilityTrialsHelper xs remainingDistractors (block :: output)


nextNewSentenceType buff dis =
    List.member dis.sentenceType (getSentenceTypes buff) |> not


isNextSentence : { a | sentenceType : Acceptability.SentenceType } -> List { b | sentenceType : Acceptability.SentenceType } -> Bool
isNextSentence dis blockBuffer =
    List.member dis.sentenceType (getSentenceTypes blockBuffer) |> not


getSentenceTypes : List { a | sentenceType : Acceptability.SentenceType } -> List Acceptability.SentenceType
getSentenceTypes sentences =
    List.map .sentenceType sentences


organizeAcceptabilityTrials : List Acceptability.Trial -> List Acceptability.Trial -> Result.Result ( Acceptability.ErrorBlock, List Acceptability.Trial ) (List (List Acceptability.Trial))
organizeAcceptabilityTrials targets distractors =
    organizeAcceptabilityTrialsHelper targets distractors []
