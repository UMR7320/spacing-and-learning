module Activity exposing (..)

import ActivityInfo exposing (ActivityInfo, Session)
import Time


type alias Trial a =
    { a | isTraining : Bool }


type Activity trial state
    = NotStarted
    | Loading (Maybe ActivityInfo) (Maybe (List trial))
    | Running Step (Data trial state)
    | Err String


type Step
    = Instructions
    | Training
    | Main


type alias Data trial state =
    { trainingTrials : List trial
    , mainTrials : List trial
    , current : Maybe trial
    , next : Maybe trial
    , state : state
    , feedback : Bool
    , history : List ( trial, state, Time.Posix )
    , infos : ActivityInfo
    }


loading : Activity t s
loading =
    Loading Nothing Nothing


infoLoaded : Session -> String -> List ActivityInfo -> s -> Activity (Trial t) s -> Activity (Trial t) s
infoLoaded session activityName infos initialState activity =
    case ActivityInfo.activityInfo infos session activityName of
        Ok info ->
            case activity of
                NotStarted ->
                    Loading (Just info) Nothing

                Loading Nothing Nothing ->
                    Loading (Just info) Nothing

                Loading Nothing (Just trials) ->
                    startIntro
                        (Ok info)
                        (List.filter (\trial -> trial.isTraining) trials)
                        (List.filter (\trial -> not trial.isTraining) trials)
                        initialState

                _ ->
                    activity

        Result.Err error ->
            Err error


trialsLoaded : List (Trial t) -> s -> Activity (Trial t) s -> Activity (Trial t) s
trialsLoaded trials initialState activity =
    case activity of
        NotStarted ->
            Loading Nothing (Just trials)

        Loading Nothing Nothing ->
            Loading Nothing (Just trials)

        Loading (Just activityInfo) Nothing ->
            startIntro
                (Ok activityInfo)
                (List.filter (\datum -> datum.isTraining) trials)
                (List.filter (\datum -> not datum.isTraining) trials)
                initialState

        _ ->
            activity


update : s -> Activity t s -> Activity t s
update newState activity =
    case activity of
        Running step data ->
            Running step { data | state = newState }

        _ ->
            Err "You can't update anything here"


type alias Identified state =
    { state | uid : String }


next : Time.Posix -> s -> Activity (Trial t) s -> Activity (Trial t) s
next timestamp resetedState activity =
    case activity of
        Running Training data ->
            case data.trainingTrials of
                _ :: y :: z :: zs ->
                    Running Training
                        { data
                            | trainingTrials = y :: z :: zs
                            , current = Just y
                            , next = Just z
                            , feedback = not data.feedback
                            , state = resetedState
                        }

                [ _, y ] ->
                    Running Training
                        { data
                            | trainingTrials = [ y ]
                            , current = Just y
                            , next = Nothing
                            , feedback = not data.feedback
                            , state = resetedState
                        }

                [ _ ] ->
                    Running Training
                        { data
                            | trainingTrials = []
                            , current = Nothing
                            , next = Nothing
                            , feedback = not data.feedback
                            , state = resetedState
                        }

                [] ->
                    Running Training
                        { data
                            | trainingTrials = []
                            , current = Nothing
                            , next = Nothing
                            , history = data.history
                            , feedback = data.feedback
                            , state = resetedState
                        }

        Running Main data ->
            case data.mainTrials of
                x :: y :: z :: zs ->
                    Running Main
                        { data
                            | mainTrials = y :: z :: zs
                            , current = Just y
                            , next = Just z
                            , history = ( x, data.state, timestamp ) :: data.history
                            , feedback = not data.feedback
                            , state = resetedState
                        }

                [ last ] ->
                    Running Main
                        { data
                            | mainTrials = []
                            , current = Nothing
                            , next = Nothing
                            , history = ( last, data.state, timestamp ) :: data.history
                            , feedback = not data.feedback
                            , state = resetedState
                        }

                [ x, y ] ->
                    Running Main
                        { data
                            | mainTrials = [ y ]
                            , current = Just y
                            , next = Nothing
                            , history = ( x, data.state, timestamp ) :: data.history
                            , feedback = not data.feedback
                            , state = resetedState
                        }

                [] ->
                    Running Main
                        { data
                            | mainTrials = []
                            , current = Nothing
                            , next = Nothing
                            , history = data.history
                            , feedback = data.feedback
                            , state = resetedState
                        }

        Err reason ->
            Err reason

        _ ->
            Err "There is no next trial to access"


{-| Init the activity with infos, trainingTrials, mainTrials and the initial state
-}
startIntro : Result String ActivityInfo -> List (Trial t) -> List (Trial t) -> s -> Activity (Trial t) s
startIntro info trainingTrials mainTrials initStat =
    case info of
        Result.Ok info_ ->
            case trainingTrials of
                [] ->
                    Running Instructions
                        { trainingTrials = trainingTrials
                        , mainTrials = mainTrials
                        , current = Nothing
                        , next = Nothing
                        , state = initStat
                        , feedback = False
                        , history = []
                        , infos = info_
                        }

                x :: y :: _ ->
                    Running Instructions
                        { trainingTrials = trainingTrials
                        , mainTrials = mainTrials
                        , current = Just x
                        , next = Just y
                        , state = initStat
                        , feedback = False
                        , history = []
                        , infos = info_
                        }

                [ x ] ->
                    Running Instructions
                        { trainingTrials = trainingTrials
                        , mainTrials = mainTrials
                        , current = Just x
                        , next = Nothing
                        , state = initStat
                        , feedback = False
                        , history = []
                        , infos = info_
                        }

        Result.Err error ->
            Err <| "I tried to start the intro of this task but I stumbled into an error : " ++ error


newStep : Step -> Activity t s -> Activity t s
newStep step activity =
    case activity of
        Running _ data ->
            Running step data

        _ ->
            Err "I can't change Step here"


startMain : Activity t s -> s -> Activity t s
startMain activity initState =
    case activity of
        Running _ data ->
            case data.mainTrials of
                x :: y :: _ ->
                    Running Main
                        { trainingTrials = []
                        , mainTrials = data.mainTrials
                        , current = Just x
                        , next = Just y
                        , state = initState
                        , feedback = False
                        , history = data.history
                        , infos = data.infos
                        }

                [ x ] ->
                    Running Main
                        { trainingTrials = []
                        , mainTrials = data.mainTrials
                        , current = Just x
                        , next = Nothing
                        , state = initState
                        , feedback = False
                        , history = data.history
                        , infos = data.infos
                        }

                [] ->
                    Running Main
                        { trainingTrials = []
                        , mainTrials = data.mainTrials
                        , current = Nothing
                        , next = Nothing
                        , state = initState
                        , feedback = False
                        , history = data.history
                        , infos = data.infos
                        }

        _ ->
            Err "I can't go to Main from here"


startTraining : Activity t s -> Activity t s
startTraining activity =
    newStep Training activity


toggle : Activity t s -> Activity t s
toggle activity =
    case activity of
        Running step data ->
            Running step { data | feedback = not data.feedback }

        _ ->
            Err "I tried to toggle the feedback but the task is still loading. Please report this error."


getState : Activity t s -> Maybe s
getState activity =
    case activity of
        Running _ { state } ->
            Just state

        _ ->
            Nothing


getTrial : Activity (Trial t) s -> Maybe (Trial t)
getTrial activity =
    case activity of
        Running _ { current } ->
            current

        _ ->
            Nothing


getHistory : Activity (Trial t) s -> List ( Trial t, s, Time.Posix )
getHistory activity =
    case activity of
        Running _ { history } ->
            history

        _ ->
            []
