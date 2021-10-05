module Main exposing (main)

import Browser
import Browser.Events exposing (onKeyDown)
import Browser.Navigation as Nav exposing (pushUrl)
import Consent
import Data
import Date
import Dict
import DnDList
import DnDList.Groups exposing (Model)
import ExperimentInfo exposing (Session(..))
import Html.Styled exposing (..)
import Html.Styled.Attributes exposing (accept, checked, class, href, type_)
import Html.Styled.Events
import Http
import Json.Decode as Decode
import Json.Decode.Pipeline exposing (optional, required)
import Json.Encode as Encode
import Logic
import Ports
import PostTests.CloudWords as CloudWords
import Pretest.Acceptability as Acceptability
import Pretest.Pretest as Pretest
import Pretest.SPR as SPR
import Pretest.SentenceCompletion as SentenceCompletion
import Pretest.VKS as VKS
import Pretest.YesNo as YesNo
import RemoteData exposing (RemoteData)
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
import Svg.Attributes exposing (xlinkHref)
import Task
import Task.Parallel as Para
import Time
import Url exposing (Url)
import Url.Builder
import Url.Parser.Query
import View exposing (navOut)


type alias Flags =
    {}



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
    , acceptabilityTask : Logic.Task Acceptability.Trial Acceptability.State
    , packetsSended : Int
    , pilote : Pilote
    , spr : SPR.SPR
    , pretest : Pretest.Pretest
    , sentenceCompletion : SentenceCompletion.SentenceCompletion
    , vks : Logic.Task VKS.Trial VKS.State
    , yesno : Logic.Task YesNo.Trial YesNo.State

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
    , cloudWords : CloudWords.CloudWords

    --                                                             .dP"Y8 88  88    db    88""Yb 888888 8888b.
    --                                                             `Ybo." 88  88   dPYb   88__dP 88__    8I  Yb
    --                                                             o.`Y8b 888888  dP__Yb  88"Yb  88""    8I  dY
    --                                                             8bodP' 88  88 dP""""Yb 88  Yb 888888 8888Y"
    , infos : RemoteData Http.Error (Dict.Dict String ExperimentInfo.Task)
    , email : String
    , user : Maybe String
    , errorTracking : List Http.Error
    , currentDate : Maybe ( Time.Zone, Time.Posix )
    , preferedStartDate : Maybe Date.Date
    , distributedSpacing : Int
    , massedSpacing : Int
    , retentionInterval : Int
    , retentionIntervalSurprise : Int
    , consent : String
    , sessions : RemoteData Http.Error (Dict.Dict String Session.Info)
    , version : Maybe String
    }


init : Flags -> Url -> Nav.Key -> ( Model, Cmd Msg )
init _ url key =
    let
        route =
            Route.fromUrl url

        version =
            Url.Parser.Query.string "version"

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
            , spr = Logic.NotStarted
            , pretest = NotAsked
            , sentenceCompletion = Logic.NotStarted
            , vks = Logic.NotStarted
            , yesno = Logic.NotStarted

            -- POSTEST
            , cloudWords = CloudWords.Running (Dict.fromList CloudWords.words)

            -- SHARED
            , user = Just ""
            , optionsOrder = [ 0, 1, 2, 3 ]
            , infos = RemoteData.Loading
            , errorTracking = []
            , email = ""
            , currentDate = Nothing
            , preferedStartDate = Nothing
            , sessions = RemoteData.Loading
            , version = Nothing
            , distributedSpacing = 7
            , massedSpacing = 2
            , retentionInterval = 14
            , retentionIntervalSurprise = 56
            , consent = ""
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
            , Cmd.batch [ Cmd.map Session1 fetchSession1, Session.getInfos ServerRespondedWithSessionsInfos ]
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
            , Data.getGeneralParemeters GotGeneralParameters
            )

        Route.TermsAndConditions ->
            ( { defaultInit
                | -- SESSION 1
                  meaning = Logic.Loading
                , spellingLvl1 = Logic.Loading
                , cu1 = Logic.Loading
                , presentation = Logic.Loading
                , user = Nothing
                , session1 = Loading loadingStateSession1
              }
            , Data.getGeneralParemeters GotGeneralParameters
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
            , Cmd.batch [ Cmd.map Session2 fetchSession2, Session.getInfos ServerRespondedWithSessionsInfos ]
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
            , Cmd.batch [ Cmd.map Session3 fetchSession3, Session.getInfos ServerRespondedWithSessionsInfos ]
            )

        Route.Pretest userid _ v ->
            let
                ( loadingStatePretest, fetchSessionPretest ) =
                    Pretest.attempt v
            in
            ( { defaultInit
                | spr = Logic.Loading
                , sentenceCompletion = Logic.Loading
                , vks = Logic.Loading
                , acceptabilityTask = Logic.Loading
                , yesno = Logic.Loading
                , user = Just userid
                , version = v
                , pretest = Loading loadingStatePretest
              }
            , Cmd.batch [ Cmd.map Pretest fetchSessionPretest, Task.perform GotCurrentTime (Task.map2 Tuple.pair Time.here Time.now), Data.getGeneralParemeters GotGeneralParameters ]
            )

        Route.Posttest _ _ ->
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
    [ View.header
        [ navOut "BCL" "https://bcl.cnrs.fr/"
        , navOut "L'équipe" "https://bcl.cnrs.fr/rubrique225"
        ]
    , View.mainEl <|
        case model.route of
            Route.Session1 _ task ->
                case task of
                    Route.TopSession1 ->
                        viewSessionInstructions model.sessions "session1" "presentation"

                    Route.Meaning ->
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

                    Route.TopSession2 ->
                        viewSessionInstructions model.sessions "session2" "translation"

            Route.AuthenticatedSession3 _ task ->
                case task of
                    Route.CU3 ->
                        [ Html.Styled.map CU3 <| CU3.view model.cu3
                        ]

                    Route.Synonym ->
                        List.map (Html.Styled.map Synonym) <| Synonym.viewTask model.synonymTask

                    Route.Spelling3 ->
                        [ Html.Styled.map Spelling3 <| Spelling3.view model.spelling3
                        ]

                    Route.TopSession3 ->
                        viewSessionInstructions model.sessions "session3" "synonym"

            Route.Posttest _ task ->
                case task of
                    Route.CloudWords ->
                        List.map (Html.Styled.map WordCloud) (CloudWords.view model)

            Route.Pretest userId task version ->
                case task of
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

                    Route.Acceptability sub ->
                        List.map (Html.Styled.map Acceptability) (Acceptability.view model.acceptabilityTask)

                    Route.YesNo ->
                        List.map (Html.Styled.map YesNo) (YesNo.view model.yesno)

                    Route.Calendar group ->
                        case model.currentDate of
                            Just ( zone, date_ ) ->
                                let
                                    date =
                                        Date.fromPosix zone date_

                                    possibleDates =
                                        Date.range Date.Day 1 (Date.add Date.Days 1 date) (Date.add Date.Days 8 date)
                                            |> List.map
                                                (\time ->
                                                    label []
                                                        [ input
                                                            [ type_ "radio"
                                                            , Html.Styled.Events.onClick (UserClickedChoseNewDate time)
                                                            , checked
                                                                (case model.preferedStartDate of
                                                                    Just d ->
                                                                        d == time

                                                                    _ ->
                                                                        False
                                                                )
                                                            ]
                                                            []
                                                        , Date.format "EEEE, d MMMM y" time |> String.append " " |> text
                                                        ]
                                                )
                                in
                                [ p [] [ text "For your convenience, you can choose the day you wish to start the next session" ]
                                , div [ class "flex flex-col" ] possibleDates
                                , case model.preferedStartDate of
                                    Nothing ->
                                        div [] []

                                    Just d ->
                                        let
                                            datesToBook =
                                                List.map
                                                    (\eachDate ->
                                                        let
                                                            formattedDate =
                                                                Date.format "EEEE, d MMMM y" eachDate
                                                        in
                                                        div [] [ text formattedDate ]
                                                    )
                                                    sessionsDates

                                            sessionsDates =
                                                [ s1, s2, s3, s4 ]

                                            spacing =
                                                case group of
                                                    Route.Massed ->
                                                        model.massedSpacing

                                                    Route.Distributed ->
                                                        model.distributedSpacing

                                            s1 =
                                                Date.add Date.Days 0 d

                                            s2 =
                                                Date.add Date.Days spacing s1

                                            s3 =
                                                Date.add Date.Days spacing s2

                                            s4 =
                                                Date.add Date.Days model.retentionInterval s3

                                            s5 =
                                                Date.add Date.Days model.retentionIntervalSurprise s3
                                        in
                                        div []
                                            ([ h3 [] [ text "Save those dates" ] ]
                                                ++ datesToBook
                                                ++ [ View.button
                                                        { message =
                                                            UserConfirmedPreferedDates
                                                                { s1 = Date.toIsoString s1
                                                                , s2 = Date.toIsoString s2
                                                                , s3 = Date.toIsoString s3
                                                                , s4 = Date.toIsoString s4
                                                                , s5 = Date.toIsoString s5
                                                                }
                                                        , txt = "I confirm I'll be available at least 1 hour each of those days"
                                                        , isDisabled = False
                                                        }
                                                   ]
                                            )
                                ]

                            Nothing ->
                                []

            Home ->
                [ div [ class "flex flex-col" ]
                    [ h1 [] [ text "Welcome to the LexLearn project!" ]
                    , div
                        [ class "mb-10 flow" ]
                        [ p [] [ text "You’re invited to participate in an online English vocabulary learning experiment. As a learner of English, you’ll be able to test your knowledge and learn some new words. As researchers, we will use your answers to help us develop online learning resources and understand better how students learn vocabulary." ]
                        , p [] [ text "To get started, you need to do two things:" ]
                        , ol
                            []
                            [ li [] [ text "Give us permission to collect your responses" ]
                            , li [] [ text "Provide a little background information" ]
                            ]
                        ]
                    , a [ class "button self-center", href "/terms-and-conditions" ] [ text "Continue" ]
                    ]
                ]

            TermsAndConditions ->
                [ div [ class "flex flex-col items-center" ]
                    [ div [ class "mb-8" ] [ View.fromMarkdown model.consent ]
                    , View.button { message = UserClickedSignInButton, txt = "Confirmer", isDisabled = False }
                    ]
                ]

            NotFound ->
                View.notFound
    ]


viewSessionInstructions : RemoteData Http.Error (Dict.Dict String Session.Info) -> String -> String -> List (Html Msg)
viewSessionInstructions remoteData sessionName url =
    case remoteData of
        RemoteData.NotAsked ->
            []

        RemoteData.Loading ->
            [ View.loading ]

        RemoteData.Success data ->
            let
                instructions =
                    Dict.get sessionName data
            in
            case instructions of
                Nothing ->
                    [ text ("I can't find instructions related to session named: " ++ sessionName) ]

                Just v ->
                    [ div [ class "instructions" ]
                        [ div [ class "pb-8" ]
                            [ View.fromMarkdown v.instructions ]
                        , a [ href url ] [ View.button { isDisabled = False, message = NoOp, txt = "Start the first activity" } ]
                        ]
                    ]

        RemoteData.Failure reason ->
            [ text (Data.buildErrorMessage reason) ]


type Msg
    = --
      --                                                           # # ### ### #    ##
      --                                                           # #  #   #  #   #
      --                                                           # #  #   #  #    #
      --                                                           # #  #   #  #     #
      --                                                           ###  #  ### ### ##
      PlaysoundInJS String
    | UserClickedLink Browser.UrlRequest
    | BrowserChangedUrl Url
    | UserClickedSignInButton
    | UserUpdatedEmailField String
    | ServerRespondedWithNewUser (Result.Result Http.Error String)
    | GotCurrentTime ( Time.Zone, Time.Posix )
    | UserClickedChoseNewDate Date.Date
    | UserConfirmedPreferedDates { s1 : String, s2 : String, s3 : String, s4 : String, s5 : String }
    | ServerRespondedWithSessionsInfos (RemoteData Http.Error (Dict.Dict String Session.Info))
    | GotGeneralParameters (Result.Result Http.Error (List Data.GeneralParameters))
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
    | SPR SPR.Msg
    | VKS VKS.Msg
    | YesNo YesNo.Msg
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
    | Synonym Synonym.Msg
    | Session3 Session3.Msg
      --
      --
      --
    | WordCloud CloudWords.Msg


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

        Acceptability submsg ->
            let
                ( newModel, newCmd ) =
                    Acceptability.update submsg model
            in
            ( newModel, Cmd.map Acceptability newCmd )

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

        YesNo submsg ->
            let
                ( subModel, subCmd ) =
                    YesNo.update submsg model
            in
            ( subModel, Cmd.map YesNo subCmd )

        WordCloud submsg ->
            let
                ( subModel, subCmd ) =
                    CloudWords.update submsg model
            in
            ( subModel, Cmd.map WordCloud subCmd )

        UserClickedSignInButton ->
            ( model
            , Nav.load "https://airtable.com/shrXTWi9PfYOkpS6Y"
            )

        UserUpdatedEmailField email ->
            ( { model | email = email }, Cmd.none )

        ServerRespondedWithNewUser (Result.Ok id) ->
            ( { model | user = Just id }, Nav.pushUrl model.key "../yes-no" )

        ServerRespondedWithNewUser (Result.Err reason) ->
            ( model, Cmd.none )

        GotCurrentTime ( zone, time ) ->
            ( { model | currentDate = Just ( zone, time ) }, Cmd.none )

        UserClickedChoseNewDate newDate ->
            ( { model | preferedStartDate = Just newDate }, Cmd.none )

        UserConfirmedPreferedDates { s1, s2, s3, s4, s5 } ->
            ( model
            , Http.request
                { url = Data.buildQuery { app = Data.apps.spacing, base = "users", view_ = "all" }
                , method = "PATCH"
                , headers = []
                , timeout = Nothing
                , tracker = Nothing
                , body =
                    Encode.list
                        (\id ->
                            Encode.object
                                [ ( "id", Encode.string id )
                                , ( "fields"
                                  , Encode.object
                                        [ ( "dateFirstEmail", Encode.string s1 )
                                        , ( "dateSecondEmail", Encode.string s2 )
                                        , ( "dateThirdEmail", Encode.string s3 )
                                        , ( "dateFourthEmail", Encode.string s4 )
                                        , ( "dateFifthEmail", Encode.string s5 )
                                        ]
                                  )
                                ]
                        )
                        [ model.user |> Maybe.withDefault "recd18l2IBRQNI05y" ]
                        |> Http.jsonBody
                , expect = Http.expectJson ServerRespondedWithNewUser decodeNewUser
                }
            )

        ServerRespondedWithSessionsInfos result ->
            ( { model | sessions = result }, Cmd.none )

        GotGeneralParameters parameters ->
            case parameters of
                Result.Err reason ->
                    ( model, Cmd.none )

                Result.Ok params ->
                    let
                        maybeGeneralParameters =
                            List.head params
                    in
                    case maybeGeneralParameters of
                        Nothing ->
                            ( model, Cmd.none )

                        Just p ->
                            ( { model
                                | distributedSpacing = p.distributedSpacing
                                , massedSpacing = p.massedSpacing
                                , retentionInterval = p.retentionInterval
                                , retentionIntervalSurprise = p.retentionIntervalSuprise
                                , consent = p.consent
                              }
                            , Cmd.none
                            )


decodeNewUser =
    Decode.field "id" Decode.string


subscriptions : Model -> Sub Msg
subscriptions model =
    Sub.batch
        [ Sub.map Spelling2 (Scrabble.subscriptions model)
        , Sub.map SPR (SPR.subscriptions model.spr)
        , Sub.map Acceptability (Acceptability.subscriptions model)
        , Sub.map YesNo (YesNo.subscriptions model)
        , Sub.map Spelling1 (SpellingLvl1.subscriptions model)
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


beep =
    "https://dl.airtable.com/.attachments/b000c72585c5f5145828b1cf3916c38d/88d9c821/beep.mp3"
