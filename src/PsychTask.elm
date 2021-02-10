module PsychTask exposing (..)


type Task trial state
    = Main
        { mainTrials : List trial
        , trainingTrials : List trial
        , current : Maybe trial
        , next : Maybe trial
        , state : state
        , feedback : Bool
        , history : List ( trial, state )
        }
    | Intr
        { trainingTrials : List trial
        , mainTrials : List trial
        , current : Maybe trial
        , next : Maybe trial
        , state : state
        , feedback : Bool
        , history : List ( trial, state )
        }
    | IntroOver
    | Over
    | NotStartedYet


update : s -> Task t s -> Task t s
update newState task =
    case task of
        Main data ->
            Main { data | state = newState }

        Intr data ->
            Intr { data | state = newState }

        Over ->
            Over

        IntroOver ->
            IntroOver

        NotStartedYet ->
            NotStartedYet


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
                    Over

        Over ->
            Over

        NotStartedYet ->
            NotStartedYet

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
                    IntroOver

        IntroOver ->
            IntroOver


startIntro : List t -> List t -> s -> Task t s
startIntro trainingTrials mainTrials initStat =
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
                }


startMain : List t -> s -> Task t s
startMain mainTrials initStat =
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
                }


toggle : Task t s -> Task t s
toggle task =
    case task of
        Main data ->
            Main { data | feedback = not data.feedback }

        Over ->
            Over

        IntroOver ->
            IntroOver

        NotStartedYet ->
            NotStartedYet

        Intr data ->
            Intr { data | feedback = not data.feedback }


getState : Task t s -> Maybe s
getState task =
    case task of
        Over ->
            Nothing

        NotStartedYet ->
            Nothing

        Main { state } ->
            Just state

        Intr { state } ->
            Just state

        IntroOver ->
            Nothing
