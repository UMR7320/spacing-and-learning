module Logic exposing (..)

import Data
import ExperimentInfo
import Http
import Json.Decode as Decode
import Json.Encode as Encode
import Task
import Time exposing (toMillis, utc)


type Task trial state
    = Main
        { mainTrials : List trial
        , trainingTrials : List trial
        , current : Maybe trial
        , next : Maybe trial
        , state : state
        , feedback : Bool
        , history : List ( trial, state )
        , infos : ExperimentInfo.Task
        }
    | Intr
        { trainingTrials : List trial
        , mainTrials : List trial
        , current : Maybe trial
        , next : Maybe trial
        , state : state
        , feedback : Bool
        , history : List ( trial, state )
        , infos : ExperimentInfo.Task
        }
    | Err String
    | Loading
    | NotStarted


saveData responseHandler maybeUserId taskId task =
    let
        history =
            getHistory task

        taskId_ =
            taskId

        callbackHandler =
            responseHandler

        userId =
            maybeUserId |> Maybe.withDefault "recd18l2IBRQNI05y"

        summarizedTrialEncoder =
            Encode.list
                (\( t, s ) ->
                    Encode.object
                        [ ( "fields"
                          , Encode.object
                                [ ( "trialUid", Encode.list Encode.string [ t.uid ] )
                                , ( "userUid", Encode.list Encode.string [ userId ] )
                                , ( "Task_UID", Encode.list Encode.string [ taskId ] )
                                , ( "attempt", Encode.string s.userAnswer )
                                ]
                          )
                        ]
                )

        sendInBatch_ =
            Data.sendInBatch summarizedTrialEncoder (Decode.field "id" Decode.string) taskId_ userId history
    in
    Task.attempt callbackHandler sendInBatch_


saveAcceptabilityData responseHandler maybeUserId taskId task =
    let
        history =
            getHistory task

        taskId_ =
            taskId

        callbackHandler =
            responseHandler

        userId =
            maybeUserId |> Maybe.withDefault "recd18l2IBRQNI05y"

        whenNothing =
            Time.millisToPosix 1000000000

        summarizedTrialEncoder =
            Encode.list
                (\( t, s ) ->
                    Encode.object
                        [ ( "fields"
                          , Encode.object
                                [ ( "trialUid", Encode.list Encode.string [ t.uid ] )
                                , ( "userUid", Encode.list Encode.string [ userId ] )
                                , ( "Task_UID", Encode.list Encode.string [ taskId ] )
                                , ( "evaluation", Encode.bool s.evaluation )
                                , ( "audioEndedAt", Encode.int (toMillis utc (s.audioEndedAt |> Maybe.withDefault whenNothing)) )
                                , ( "beepEndedAt", Encode.int (toMillis utc (s.beepEndedAt |> Maybe.withDefault whenNothing)) )
                                , ( "userAnsweredAt", Encode.int (toMillis utc (s.userAnsweredAt |> Maybe.withDefault whenNothing)) )
                                , ( "evaluation", Encode.bool s.evaluation )
                                ]
                          )
                        ]
                )

        sendInBatch_ =
            Data.sendInBatch summarizedTrialEncoder (Decode.field "id" Decode.string) taskId_ userId history
    in
    Task.attempt callbackHandler sendInBatch_


type alias Infos =
    Result String ExperimentInfo.Task


update : s -> Task t s -> Task t s
update newState task =
    case task of
        Main data ->
            Main { data | state = newState }

        Intr data ->
            Intr { data | state = newState }

        Loading ->
            Err "You can't update anything while loading the data. Please report this error."

        Err reason ->
            Err (reason ++ "|| You can't update anything when you are in an error state. Please reports those errors")

        NotStarted ->
            Err "You can't update anything until the task is started. Please report this error"


type alias Identified state =
    { state | uid : String }


next : s -> Task t s -> Task t s
next resetedState task =
    case task of
        Main data ->
            case data.mainTrials of
                x :: y :: z :: zs ->
                    Main
                        { data
                            | mainTrials = y :: z :: zs
                            , current = Just y
                            , next = Just z
                            , history = ( x, data.state ) :: data.history
                            , feedback = not data.feedback
                            , state = resetedState
                        }

                [ last ] ->
                    Main
                        { data
                            | mainTrials = []
                            , current = Nothing
                            , next = Nothing
                            , history = ( last, data.state ) :: data.history
                            , feedback = not data.feedback
                            , state = resetedState
                        }

                [ x, y ] ->
                    Main
                        { data
                            | mainTrials = [ y ]
                            , current = Just y
                            , next = Nothing
                            , history = ( x, data.state ) :: data.history
                            , feedback = not data.feedback
                            , state = resetedState
                        }

                [] ->
                    Err "I tried to move to the next trial but there is no more trial.  Please report this error message."

        Loading ->
            Err "You can't go to the next trial before the experiment is started. Please report this error message."

        NotStarted ->
            Err "You can't go to the next trial before the experiment is started. Please report this error message."

        Err reason ->
            Err reason

        Intr data ->
            case data.trainingTrials of
                x :: y :: z :: zs ->
                    Intr
                        { data
                            | trainingTrials = y :: z :: zs
                            , current = Just y
                            , next = Just z
                            , history = ( x, data.state ) :: data.history
                            , feedback = not data.feedback
                            , state = resetedState
                        }

                [ last ] ->
                    Intr
                        { data
                            | trainingTrials = []
                            , current = Nothing
                            , next = Nothing
                            , history = ( last, data.state ) :: data.history
                            , feedback = not data.feedback
                            , state = resetedState
                        }

                [ x, y ] ->
                    Intr
                        { data
                            | trainingTrials = [ y ]
                            , current = Just y
                            , next = Nothing
                            , history = ( x, data.state ) :: data.history
                            , feedback = not data.feedback
                            , state = resetedState
                        }

                [] ->
                    Err "There is no more trial"


startIntro : Infos -> List t -> List t -> s -> Task t s
startIntro infos trainingTrials mainTrials initStat =
    case infos of
        Result.Ok info ->
            case trainingTrials of
                [] ->
                    Intr
                        { trainingTrials = trainingTrials
                        , mainTrials = mainTrials
                        , current = Nothing
                        , next = Nothing
                        , state = initStat
                        , feedback = False
                        , history = []
                        , infos = info
                        }

                x :: y :: _ ->
                    Intr
                        { trainingTrials = trainingTrials
                        , mainTrials = mainTrials
                        , current = Just x
                        , next = Just y
                        , state = initStat
                        , feedback = False
                        , history = []
                        , infos = info
                        }

                [ x ] ->
                    Intr
                        { trainingTrials = trainingTrials
                        , mainTrials = mainTrials
                        , current = Just x
                        , next = Nothing
                        , state = initStat
                        , feedback = False
                        , history = []
                        , infos = info
                        }

        Result.Err error ->
            Err <| "I tried to start the intro of this task but I stumbled into an error : " ++ error


startMain : ExperimentInfo.Task -> List t -> s -> Task t s
startMain info mainTrials initStat =
    case mainTrials of
        [] ->
            Main
                { trainingTrials = []
                , mainTrials = []
                , current = Nothing
                , next = Nothing
                , state = initStat
                , feedback = False
                , history = []
                , infos = info
                }

        x :: y :: ys ->
            Main
                { trainingTrials = []
                , mainTrials = y :: ys
                , current = Just x
                , next = Just y
                , state = initStat
                , feedback = False
                , history = []
                , infos = info
                }

        [ x ] ->
            Main
                { trainingTrials = []
                , mainTrials = mainTrials
                , current = Just x
                , next = Nothing
                , state = initStat
                , feedback = False
                , history = []
                , infos = info
                }


toggle : Task t s -> Task t s
toggle task =
    case task of
        Main data ->
            Main { data | feedback = not data.feedback }

        Loading ->
            Err "I tried to toggle the feedback but the task is still loading. Please report this error."

        NotStarted ->
            Err "I tried to toggle the feedback but the task is not started. Please report this error."

        Err reason ->
            Err (reason ++ "|| I tried to toggle the feedback but the task is in its error state. Please report this error.")

        Intr data ->
            Intr { data | feedback = not data.feedback }


getState : Task t s -> Maybe s
getState task =
    case task of
        Main { state } ->
            Just state

        Intr { state } ->
            Just state

        _ ->
            Nothing


getTrial : Task t s -> Maybe t
getTrial task =
    case task of
        Main { current } ->
            current

        Intr { current } ->
            current

        _ ->
            Nothing


getHistory : Task t s -> List ( t, s )
getHistory task =
    case task of
        Main { history } ->
            history

        _ ->
            []
