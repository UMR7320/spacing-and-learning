module Main exposing (main)

import Activity
import ActivityInfo exposing (ActivityInfo)
import Browser
import Browser.Navigation as Nav
import Data exposing (UserCanParticipate(..))
import Date
import Dict
import DnDList
import DnDList.Groups exposing (Model)
import Html.Styled exposing (..)
import Html.Styled.Attributes exposing (checked, class, href, type_)
import Html.Styled.Events
import Http
import Json.Decode as Decode
import Json.Encode as Encode
import Ports
import PostTests.CloudWords as CloudWords
import Pretest.Acceptability as Acceptability exposing (Acceptability)
import Pretest.Pretest as Pretest
import Pretest.SPR as SPR exposing (SPR)
import Pretest.SentenceCompletion as SentenceCompletion exposing (SentenceCompletion)
import Pretest.VKS as VKS exposing (VKS)
import Pretest.Version exposing (Version(..))
import Pretest.YesNo as YesNo exposing (YesNo)
import ProgressBar
import RemoteData exposing (RemoteData)
import Route exposing (Route(..), Session1Activity(..), Session2Activity(..))
import Session exposing (Session(..))
import Session1.Context1 as Context1 exposing (Context1)
import Session1.Meaning1 as Meaning1 exposing (Meaning1)
import Session1.Presentation as Presentation exposing (Presentation)
import Session1.Spelling1 as Spelling1 exposing (Spelling1)
import Session2.Context2 as Context2 exposing (Context2)
import Session2.Meaning2 as Meaning2 exposing (Meaning2)
import Session2.Spelling2 as Spelling2 exposing (Spelling2)
import Session3.Context3 as Context3 exposing (Context3)
import Session3.Meaning3 as Meaning3 exposing (Meaning3)
import Session3.Spelling3 as Spelling3 exposing (Spelling3)
import Task
import Time
import Url exposing (Url)
import Url.Builder
import UserCode
import View exposing (navOut)


type alias Flags =
    String



-- MODEL


type alias Model =
    { -- Utils
      key : Nav.Key
    , route : Route.Route
    , optionsOrder : List Int

    -- dnd stands for drag-and-drop
    , dnd : DnDList.Model

    -- PreTest
    , pretest : Pretest.Pretest
    , acceptability : Acceptability
    , spr : SPR
    , sentenceCompletion : SentenceCompletion
    , vks : VKS
    , yesNo : YesNo

    -- Session 1
    , presentation : Presentation
    , meaning1 : Meaning1
    , spelling1 : Spelling1
    , context1 : Context1
    , endAcceptabilityDuration : Int

    -- Session 2
    , meaning2 : Meaning2
    , spelling2 : Spelling2
    , context2 : Context2

    -- Session 3
    , meaning3 : Meaning3
    , spelling3 : Spelling3
    , context3 : Context3

    -- PostTest
    , cloudWords : CloudWords.CloudWords

    -- Shared
    , activitiesInfos : RemoteData Http.Error (List ActivityInfo)
    , email : String
    , user : Maybe String
    , errorTracking : List Http.Error
    , currentDate : Maybe ( Time.Zone, Time.Posix )
    , preferedStartDate : Maybe Date.Date
    , sessions : RemoteData Http.Error (Dict.Dict String Session.Info)
    , version : Version
    , session : Maybe String
    , generalParameters : RemoteData Http.Error Data.GeneralParameters
    , userCanParticipate : RemoteData Http.Error UserCanParticipate
    , backgroundQuestionnaireUrl : String

    -- User Code
    , userCode : UserCode.Model
    }


defaultModel : Nav.Key -> Route.Route -> String -> Model
defaultModel key route backgroundQuestionnaireUrl =
    { key = key
    , route = route
    , dnd = Spelling2.system.model

    -- SESSION 1
    , presentation = Activity.NotStarted
    , meaning1 = Activity.NotStarted
    , spelling1 = Activity.NotStarted
    , context1 = Activity.NotStarted

    -- SESSION 2
    , meaning2 = Activity.NotStarted
    , spelling2 = Activity.NotStarted
    , context2 = Activity.NotStarted

    -- SESSION 3
    , meaning3 = Activity.NotStarted
    , spelling3 = Activity.NotStarted
    , context3 = Activity.NotStarted

    -- PILOTE
    , acceptability = Activity.NotStarted
    , endAcceptabilityDuration = 6000

    -- PRETEST
    , spr = Activity.NotStarted
    , pretest = NotAsked
    , sentenceCompletion = Activity.NotStarted
    , vks = Activity.NotStarted
    , yesNo = Activity.NotStarted

    -- POSTEST
    , cloudWords = CloudWords.Loading

    -- UserCode
    , userCode = UserCode.emptyModel

    -- SHARED
    , user = Nothing
    , optionsOrder = [ 0, 1, 2, 3 ]
    , activitiesInfos = RemoteData.Loading
    , errorTracking = []
    , email = ""
    , currentDate = Nothing
    , preferedStartDate = Nothing
    , sessions = RemoteData.Loading
    , version = PreTest
    , session = Nothing
    , generalParameters = RemoteData.Loading
    , userCanParticipate = RemoteData.NotAsked
    , backgroundQuestionnaireUrl = backgroundQuestionnaireUrl
    }


init : Flags -> Url -> Nav.Key -> ( Model, Cmd Msg )
init backgroundQuestionnaireUrl url key =
    let
        route =
            Route.fromUrl url

        ( model, routeCmd ) =
            defaultModel key route backgroundQuestionnaireUrl
                |> changeRouteTo route

        cmd =
            Cmd.batch
                [ routeCmd
                , Task.attempt (RemoteData.fromResult >> ServerRespondedWithActivitiesInfos) ActivityInfo.getRecords
                ]
    in
    case route of
        Route.Home ->
            ( model
            , Cmd.batch [ cmd, Data.getGeneralParameters GotGeneralParameters ]
            )

        Route.TermsAndConditions ->
            ( model
            , Cmd.batch [ cmd, Data.getGeneralParameters GotGeneralParameters ]
            )

        Route.Pretest userid _ v ->
            let
                ( loadingStatePretest, fetchSessionPretest ) =
                    Pretest.attempt v
            in
            ( { model
                | spr = Activity.loading model.spr
                , sentenceCompletion = Activity.loading model.sentenceCompletion
                , vks = Activity.loading model.vks
                , acceptability = Activity.loading model.acceptability
                , yesNo = Activity.loading model.yesNo
                , user = Just userid
                , version = v
                , pretest = Loading loadingStatePretest
                , userCanParticipate = RemoteData.Loading
              }
            , Cmd.batch
                [ cmd
                , Cmd.map Pretest fetchSessionPretest
                , Task.perform GotCurrentTime (Task.map2 Tuple.pair Time.here Time.now)
                , Data.getGeneralParameters GotGeneralParameters
                ]
            )

        Route.Session1 userId _ ->
            ( { model | user = Just userId }
            , Cmd.batch
                [ cmd
                , Session.getInfos ServerRespondedWithSessionsInfos
                ]
            )

        Route.Session2 userid _ ->
            ( { model | user = Just userid }
            , Cmd.batch
                [ cmd
                , Session.getInfos ServerRespondedWithSessionsInfos
                ]
            )

        Route.Session3 userid _ ->
            ( { model | user = Just userid }
            , Cmd.batch
                [ cmd
                , Session.getInfos ServerRespondedWithSessionsInfos
                ]
            )

        Route.CalendarUpdated ->
            ( { model | route = CalendarUpdated }, cmd )

        Route.UserCode _ ->
            ( model, cmd )

        NotFound ->
            ( { model | route = NotFound }, cmd )



-- VIEW


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
    , ProgressBar.view model
    , View.mainEl <|
        case model.route of
            Route.Session1 _ activity ->
                case activity of
                    Route.TopSession1 ->
                        viewSessionInstructions model.sessions "session1" "session1/presentation"

                    Route.Presentation ->
                        [ Html.Styled.map Presentation (Presentation.view model.presentation) ]

                    Route.Meaning1 ->
                        [ Html.Styled.map Meaning1 (Meaning1.view model) ]

                    Route.Spelling1 ->
                        [ Html.Styled.map Spelling1 (Spelling1.view model) ]

                    Route.Context1 ->
                        [ Html.Styled.map Context1 (Context1.view model) ]

                    Route.WordCloud1 ->
                        List.map (Html.Styled.map WordCloud) (CloudWords.view "S1" model)

            Route.Session2 _ activity ->
                case activity of
                    Route.TopSession2 ->
                        viewSessionInstructions model.sessions "session2" "session2/meaning"

                    Route.Meaning2 ->
                        [ Html.Styled.map Meaning2 (Meaning2.view model) ]

                    Route.Spelling2 ->
                        [ Html.Styled.map Spelling2 (Spelling2.view model) ]

                    Route.Context2 ->
                        [ Html.Styled.map Context2 (Context2.view model) ]

                    Route.WordCloud2 ->
                        List.map (Html.Styled.map WordCloud) (CloudWords.view "S2" model)

            Route.Session3 _ activity ->
                case activity of
                    Route.TopSession3 ->
                        viewSessionInstructions model.sessions "session3" "session3/meaning"

                    Route.Meaning3 ->
                        List.map (Html.Styled.map Meaning3) <| Meaning3.viewActivity model.meaning3

                    Route.Spelling3 ->
                        [ Html.Styled.map Spelling3 <| Spelling3.view model.spelling3 ]

                    Route.Context3 ->
                        [ Html.Styled.map Context3 <| Context3.view model ]

                    Route.WordCloud3 ->
                        List.map (Html.Styled.map WordCloud) (CloudWords.view "S3" model)

            Route.Pretest _ subPage version ->
                case model.userCanParticipate of
                    RemoteData.NotAsked ->
                        [ View.loading ]

                    RemoteData.Loading ->
                        [ View.loading ]

                    RemoteData.Failure error ->
                        [ text (Data.buildErrorMessage error) ]

                    RemoteData.Success userCanParticipate ->
                        case userCanParticipate of
                            No reason ->
                                [ text reason ]

                            Yes _ ->
                                case subPage of
                                    Route.TopPretest ->
                                        let
                                            ( sessionName, firstActivityLink ) =
                                                if version == PostTestDiff then
                                                    -- this name is hardcoded in Airtable
                                                    ( "Post-test-diff", "post-test-diff/vks" )

                                                else
                                                    ( "pretest", "pretest/yes-no" )
                                        in
                                        viewSessionInstructions model.sessions sessionName firstActivityLink

                                    Route.EmailSent ->
                                        [ text "Un email a été envoyé. Veuillez cliquer sur le lien pour continuer l'expérience." ]

                                    Route.SPR ->
                                        List.map (Html.Styled.map SPR) (SPR.view model.spr)

                                    Route.SentenceCompletion ->
                                        List.map (Html.Styled.map SentenceCompletion) (SentenceCompletion.view model.sentenceCompletion)

                                    Route.GeneralInfos ->
                                        []

                                    Route.VKS page ->
                                        List.map (Html.Styled.map VKS) (VKS.view model page)

                                    Route.Acceptability _ ->
                                        List.map (Html.Styled.map Acceptability) (Acceptability.view model.acceptability)

                                    Route.YesNo page ->
                                        List.map (Html.Styled.map YesNo) (YesNo.view model.yesNo page)

                                    Route.Calendar _ group ->
                                        case model.generalParameters of
                                            RemoteData.NotAsked ->
                                                [ View.loading ]

                                            RemoteData.Loading ->
                                                [ View.loading ]

                                            RemoteData.Failure error ->
                                                [ text (Data.buildErrorMessage error) ]

                                            RemoteData.Success parameters ->
                                                viewCalendar model group parameters

            Home ->
                [ div [ class "flex flex-col" ]
                    [ h1 [] [ text "Welcome to the LexLearn project!" ]
                    , div
                        [ class "mb-10 flow" ]
                        [ p [] [ text "Merci de votre intérêt, malheureusement nous n'acceptons plus de nouvelles participations pour le moment." ]
                        ]
                    ]
                ]

            TermsAndConditions ->
                case model.generalParameters of
                    RemoteData.NotAsked ->
                        [ View.loading ]

                    RemoteData.Loading ->
                        [ View.loading ]

                    RemoteData.Failure error ->
                        [ text (Data.buildErrorMessage error) ]

                    RemoteData.Success parameters ->
                        [ div [ class "flex flex-col items-center" ]
                            [ div [ class "mb-8" ] [ View.fromMarkdown parameters.consent ]
                            , View.button { message = UserClickedSignInButton, txt = "Confirmer", isDisabled = False }
                            ]
                        ]

            CalendarUpdated ->
                [ text "Your planning has been updated!" ]

            Route.UserCode _ ->
                List.map (Html.Styled.map UserCode) (UserCode.view model.userCode)

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
                        , a [ href url ] [ View.button { isDisabled = False, message = NoOp, txt = "C'est parti !" } ]
                        ]
                    ]

        RemoteData.Failure reason ->
            [ text (Data.buildErrorMessage reason) ]


viewCalendar : Model -> Route.Group -> Data.GeneralParameters -> List (Html Msg)
viewCalendar model group generalParameters =
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
            [ div [ class "flow calendar" ]
                [ h1 [] [ text "Planning" ]
                , p [] [ text "Before starting today, choose your personalised calendar for your LexLearn sessions. Click on a starting date to see which four days you need to be available. If some days don't suit, click on a different starting date." ]
                , div [ class "possible-dates" ] possibleDates
                , case model.preferedStartDate of
                    Nothing ->
                        text ""

                    Just d ->
                        let
                            datesToBook =
                                List.map
                                    (\eachDate ->
                                        let
                                            formattedDate =
                                                Date.format "EEEE, d MMMM y" eachDate
                                        in
                                        li [] [ text formattedDate ]
                                    )
                                    sessionsDates
                                    |> ul [ class "dates-to-book" ]

                            sessionsDates =
                                [ s1, s2, s3, s4 ]

                            spacing =
                                case group of
                                    Route.Massed ->
                                        generalParameters.massedSpacing

                                    Route.Distributed ->
                                        generalParameters.distributedSpacing

                            s1 =
                                Date.add Date.Days 0 d

                            s2 =
                                Date.add Date.Days spacing s1

                            s3 =
                                Date.add Date.Days spacing s2

                            s4 =
                                Date.add Date.Days generalParameters.retentionInterval s3

                            s5 =
                                Date.add Date.Days generalParameters.retentionIntervalSurprise s3
                        in
                        div [ class "flow" ]
                            [ p [] [ text "You need to be available at least one hour on each of these days" ]
                            , datesToBook
                            , View.button
                                { message =
                                    UserConfirmedPreferedDates
                                        { s1 = Date.toIsoString s1
                                        , s2 = Date.toIsoString s2
                                        , s3 = Date.toIsoString s3
                                        , s4 = Date.toIsoString s4
                                        , s5 = Date.toIsoString s5
                                        }
                                , txt = "Continue"
                                , isDisabled = False
                                }
                            ]
                ]
            ]

        Nothing ->
            []



-- UPDATE


type Msg
    = PlaysoundInJS String
    | UserClickedLink Browser.UrlRequest
    | BrowserChangedUrl Url
    | UserClickedSignInButton
    | UserUpdatedEmailField String
    | ServerRespondedWithNewUser (Result.Result Http.Error String)
    | GotCurrentTime ( Time.Zone, Time.Posix )
    | UserClickedChoseNewDate Date.Date
    | UserConfirmedPreferedDates { s1 : String, s2 : String, s3 : String, s4 : String, s5 : String }
    | ServerRespondedWithSessionsInfos (RemoteData Http.Error (Dict.Dict String Session.Info))
    | ServerRespondedWithActivitiesInfos (RemoteData Http.Error (List ActivityInfo))
    | GotGeneralParameters (RemoteData Http.Error Data.GeneralParameters)
    | GotCanUserParticipate Route (RemoteData Http.Error UserCanParticipate)
    | NoOp
      -- PreTest
    | Acceptability Acceptability.Msg
    | Pretest Pretest.Msg
    | SentenceCompletion SentenceCompletion.Msg
    | SPR SPR.Msg
    | VKS VKS.Msg
    | YesNo YesNo.Msg
      -- Session 1
    | Presentation Presentation.Msg
    | Context1 Context1.Msg
    | Meaning1 Meaning1.Msg
    | Spelling1 Spelling1.Msg
      -- Session 2
    | Meaning2 Meaning2.Msg
    | Spelling2 Spelling2.Msg
    | Context2 Context2.Msg
      -- Session 3
    | Meaning3 Meaning3.Msg
    | Spelling3 Spelling3.Msg
    | Context3 Context3.Msg
      -- WordCloud
    | WordCloud CloudWords.Msg
      -- User Code
    | UserCode UserCode.Msg


changeRouteTo : Route -> Model -> ( Model, Cmd Msg )
changeRouteTo route model =
    let
        newModel =
            { model | route = route }
    in
    case route of
        Route.Home ->
            ( newModel, Cmd.none )

        Route.TermsAndConditions ->
            ( newModel, Cmd.none )

        Route.NotFound ->
            ( newModel, Cmd.none )

        Route.Pretest userId pretestRoute version ->
            let
                updatedModel =
                    { newModel
                        | user = Just userId
                        , version = version
                    }
            in
            case ( pretestRoute, updatedModel.userCanParticipate ) of
                ( _, RemoteData.NotAsked ) ->
                    ( updatedModel
                    , Data.getCanUserParticipate userId (GotCanUserParticipate route)
                    )

                ( Route.TopPretest, _ ) ->
                    ( updatedModel
                    , Session.getInfos ServerRespondedWithSessionsInfos
                    )

                ( Route.YesNo _, RemoteData.Success (Yes _) ) ->
                    ( updatedModel
                    , Cmd.batch
                        [ Cmd.map YesNo YesNo.getRecords
                        , Ports.enableAlertOnExit ()
                        ]
                    )

                ( Route.VKS _, RemoteData.Success (Yes group) ) ->
                    ( updatedModel
                    , Cmd.batch
                        [ Cmd.map VKS (VKS.getRecords group)
                        , Ports.enableAlertOnExit ()
                        ]
                    )

                _ ->
                    ( updatedModel, Cmd.none )

        Route.Session1 userId session1Activity ->
            let
                updatedModel =
                    { newModel | user = Just userId }
            in
            case ( session1Activity, updatedModel.userCanParticipate ) of
                ( Route.TopSession1, _ ) ->
                    ( updatedModel
                    , Session.getInfos ServerRespondedWithSessionsInfos
                    )

                ( _, RemoteData.NotAsked ) ->
                    ( updatedModel
                    , Data.getCanUserParticipate userId (GotCanUserParticipate route)
                    )

                ( Route.Presentation, RemoteData.Success (Yes group) ) ->
                    Presentation.init group updatedModel
                        |> updateWith Presentation

                ( Route.Meaning1, RemoteData.Success (Yes group) ) ->
                    Meaning1.init group updatedModel
                        |> updateWith Meaning1

                ( Route.Spelling1, RemoteData.Success (Yes group) ) ->
                    Spelling1.init group updatedModel
                        |> updateWith Spelling1

                ( Route.Context1, RemoteData.Success (Yes group) ) ->
                    Context1.init group updatedModel
                        |> updateWith Context1

                ( Route.WordCloud1, RemoteData.Success (Yes group) ) ->
                    CloudWords.init group updatedModel
                        |> updateWith WordCloud

                _ ->
                    ( updatedModel, Cmd.none )

        Route.Session2 userId session2Activity ->
            let
                updatedModel =
                    { newModel | user = Just userId }
            in
            case ( session2Activity, updatedModel.userCanParticipate ) of
                ( Route.TopSession2, _ ) ->
                    ( updatedModel
                    , Session.getInfos ServerRespondedWithSessionsInfos
                    )

                ( _, RemoteData.NotAsked ) ->
                    ( updatedModel
                    , Data.getCanUserParticipate userId (GotCanUserParticipate route)
                    )

                ( Route.Meaning2, RemoteData.Success (Yes group) ) ->
                    Meaning2.init group updatedModel
                        |> updateWith Meaning2

                ( Route.Spelling2, RemoteData.Success (Yes group) ) ->
                    Spelling2.init group updatedModel
                        |> updateWith Spelling2

                ( Route.Context2, RemoteData.Success (Yes group) ) ->
                    Context2.init group updatedModel
                        |> updateWith Context2

                ( Route.WordCloud2, RemoteData.Success (Yes group) ) ->
                    CloudWords.init group updatedModel
                        |> updateWith WordCloud

                _ ->
                    ( updatedModel, Cmd.none )

        Route.Session3 userId session3Activity ->
            let
                updatedModel =
                    { newModel | user = Just userId }
            in
            case ( session3Activity, updatedModel.userCanParticipate ) of
                ( Route.TopSession3, _ ) ->
                    ( updatedModel
                    , Session.getInfos ServerRespondedWithSessionsInfos
                    )

                ( _, RemoteData.NotAsked ) ->
                    ( updatedModel
                    , Data.getCanUserParticipate userId (GotCanUserParticipate route)
                    )

                ( Route.Meaning3, RemoteData.Success (Yes group) ) ->
                    Meaning3.init group updatedModel
                        |> updateWith Meaning3

                ( Route.Spelling3, RemoteData.Success (Yes group) ) ->
                    Spelling3.init group updatedModel
                        |> updateWith Spelling3

                ( Route.Context3, RemoteData.Success (Yes group) ) ->
                    Context3.init group updatedModel
                        |> updateWith Context3

                ( Route.WordCloud3, RemoteData.Success (Yes group) ) ->
                    CloudWords.init group updatedModel
                        |> updateWith WordCloud

                _ ->
                    ( updatedModel, Cmd.none )

        Route.CalendarUpdated ->
            ( newModel, Cmd.none )

        Route.UserCode maybeDate ->
            let
                userCodeModel =
                    newModel.userCode

                newUserCodeModel =
                    { userCodeModel | date = maybeDate }
            in
            ( { newModel | userCode = newUserCodeModel }, Cmd.none )


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        BrowserChangedUrl url ->
            changeRouteTo (Route.fromUrl url) model

        NoOp ->
            ( model, Cmd.none )

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
            SPR.update submsg model
                |> updateWith SPR

        Pretest submsg ->
            Pretest.update submsg model
                |> updateWith Pretest

        SentenceCompletion submsg ->
            SentenceCompletion.update submsg model
                |> updateWith SentenceCompletion

        VKS submsg ->
            VKS.update submsg model
                |> updateWith VKS

        Acceptability submsg ->
            Acceptability.update submsg model
                |> updateWith Acceptability

        PlaysoundInJS url ->
            ( model, Ports.playAudio url )

        Presentation submsg ->
            Presentation.update submsg model
                |> updateWith Presentation

        Meaning1 submsg ->
            Meaning1.update submsg model
                |> updateWith Meaning1

        Meaning2 submsg ->
            Meaning2.update submsg model
                |> updateWith Meaning2

        Meaning3 submsg ->
            Meaning3.update submsg model
                |> updateWith Meaning3

        Spelling1 submsg ->
            Spelling1.update submsg model
                |> updateWith Spelling1

        Spelling2 submsg ->
            Spelling2.update submsg model
                |> updateWith Spelling2

        Spelling3 submsg ->
            Spelling3.update submsg model
                |> updateWith Spelling3

        Context1 submsg ->
            Context1.update submsg model
                |> updateWith Context1

        Context2 submsg ->
            Context2.update submsg model
                |> updateWith Context2

        Context3 submsg ->
            Context3.update submsg model
                |> updateWith Context3

        YesNo submsg ->
            YesNo.update submsg model
                |> updateWith YesNo

        WordCloud submsg ->
            CloudWords.update submsg model
                |> updateWith WordCloud

        UserClickedSignInButton ->
            ( model
            , Nav.load model.backgroundQuestionnaireUrl
            )

        UserUpdatedEmailField email ->
            ( { model | email = email }, Cmd.none )

        ServerRespondedWithNewUser (Result.Ok id) ->
            ( { model | user = Just id }
            , case model.route of
                Route.Pretest _ (Route.Calendar False _) _ ->
                    Nav.pushUrl model.key "../yes-no"

                _ ->
                    Nav.pushUrl model.key "/calendar-updated"
            )

        ServerRespondedWithNewUser (Result.Err _) ->
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
                                        , ( "isFirstEmailSent", Encode.bool False )
                                        , ( "isSecondEmailSent", Encode.bool False )
                                        , ( "isThirdEmailSent", Encode.bool False )
                                        , ( "isFourthEmailSent", Encode.bool False )
                                        , ( "isSurpriseEmailSent", Encode.bool False )
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

        ServerRespondedWithActivitiesInfos (RemoteData.Success infos) ->
            ( { model
                | presentation = Presentation.infoLoaded infos model.presentation
                , meaning1 = Meaning1.infoLoaded infos model.meaning1
                , meaning2 = Meaning2.infoLoaded infos model.meaning2
                , meaning3 = Meaning3.infoLoaded infos model.meaning3
                , spelling1 = Spelling1.infoLoaded infos model.spelling1
                , spelling2 = Spelling2.infoLoaded infos model.spelling2
                , spelling3 = Spelling3.infoLoaded infos model.spelling3
                , context1 = Context1.infoLoaded infos model.context1
                , context2 = Context2.infoLoaded infos model.context2
                , context3 = Context3.infoLoaded infos model.context3
                , acceptability = Acceptability.infoLoaded infos model.version model.acceptability
                , spr = SPR.infoLoaded infos model.version model.spr
                , sentenceCompletion = SentenceCompletion.infoLoaded infos model.version model.sentenceCompletion
                , yesNo = YesNo.infoLoaded infos model.yesNo
                , vks = VKS.infoLoaded infos model.version model.vks
              }
            , Cmd.none
            )

        ServerRespondedWithActivitiesInfos (RemoteData.Failure _) ->
            ( { model
                | presentation = Activity.Err "Error loading the activity information"
                , meaning1 = Activity.Err "Error loading the activity information"
                , spelling1 = Activity.Err "Error loading the activity information"
                , context1 = Activity.Err "Error loading the activity information"
                , meaning2 = Activity.Err "Error loading the activity information"
                , spelling2 = Activity.Err "Error loading the activity information"
                , context2 = Activity.Err "Error loading the activity information"
                , meaning3 = Activity.Err "Error loading the activity information"
                , spelling3 = Activity.Err "Error loading the activity information"
                , context3 = Activity.Err "Error loading the activity information"
                , acceptability = Activity.Err "Error loading the activity information"
                , spr = Activity.Err "Error loading the activity information"
                , sentenceCompletion = Activity.Err "Error loading the activity information"
                , yesNo = Activity.Err "Error loading the activity information"
              }
            , Cmd.none
            )

        ServerRespondedWithActivitiesInfos _ ->
            ( model, Cmd.none )

        GotGeneralParameters parameters ->
            ( { model | generalParameters = parameters }
            , Cmd.none
            )

        GotCanUserParticipate route userCanParticipate ->
            -- re-run the initialisation of the original route now that we've logged in our user
            changeRouteTo route { model | userCanParticipate = userCanParticipate }

        UserCode submsg ->
            UserCode.update submsg model
                |> updateWith UserCode


updateWith : (a -> msg) -> ( b, Cmd a ) -> ( b, Cmd msg )
updateWith subMsg ( model, subCmd ) =
    ( model, Cmd.map subMsg subCmd )


decodeNewUser : Decode.Decoder String
decodeNewUser =
    Decode.field "id" Decode.string


subscriptions : Model -> Sub Msg
subscriptions model =
    Sub.batch
        [ Sub.map Spelling2 (Spelling2.subscriptions model)
        , Sub.map SPR (SPR.subscriptions model.spr)
        , Sub.map Acceptability (Acceptability.subscriptions model)
        , Sub.map Spelling1 (Spelling1.subscriptions model)
        ]


project : { description : String, title : String, url : String }
project =
    { title = "Apprentissage et espacement"
    , description = """
        Une expérience visant à mieux comprendre l'acquisition de nouvelles structures grammaticales en langue anglaise.
      """
    , url = Url.Builder.absolute [ "start" ] []
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
