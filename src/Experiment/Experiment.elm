module Experiment.Experiment exposing (..)

--import Experiment.Acceptability exposing (Trial)

import Array
import Css exposing (end)
import Data
import Html exposing (a)
import Http
import Json.Decode as Decode
import RemoteData exposing (WebData)
import Url.Builder


buildQuery : AirtableQueryParameters -> String
buildQuery { app, base, view_ } =
    Url.Builder.absolute
        [ ".netlify"
        , "functions"
        , "api"
        ]
        [ Url.Builder.string "app" app
        , Url.Builder.string "base" base
        , Url.Builder.string "view" view_
        ]


apps : { spacing : String, sleep : String }
apps =
    { spacing = "appvKOc8FH0j48Hw1"
    , sleep = "appTEVHZLw3jNa7fU"
    }


type alias AirtableQueryParameters =
    { app : String
    , base : String
    , view_ : String
    }


nextTrial : Experiment -> Experiment
nextTrial experiment =
    case experiment of
        DoingMeaning (MainLoop trials state ntrial feedback) ->
            if ntrial >= List.length trials - 1 then
                DoingMeaning (End "This is the end")

            else
                DoingMeaning (MainLoop trials state (ntrial + 1) feedback)

        DoingTranslation (MainLoop trials state ntrial feedback) ->
            if ntrial >= List.length trials - 1 then
                DoingTranslation (End "This is the end")

            else
                DoingTranslation (MainLoop trials state (ntrial + 1) feedback)

        DoingScrabble (MainLoop trials state ntrial feedback) ->
            if ntrial >= List.length trials - 1 then
                DoingScrabble (End "This is the end")

            else
                DoingScrabble (MainLoop trials state (ntrial + 1) feedback)

        DoingSynonym (MainLoop trials state ntrial feedback) ->
            if ntrial >= List.length trials - 1 then
                DoingSynonym (End "this is the end of synonym")

            else
                DoingSynonym (MainLoop trials state (ntrial + 1) feedback)

        DoingSynonym (Intro trials state ntrial feedback instructions) ->
            if ntrial >= List.length trials - 1 then
                DoingSynonym (MainLoop trials state 0 False)

            else
                DoingSynonym (Intro trials state (ntrial + 1) feedback instructions)

        _ ->
            Failure
                (Http.BadBody
                    """
            I tried to go to next trial but I ended into an ignored case.
            Please report this error. If you have access to the source code, update the Experiment.nextTrial function to take this case in account. 
            """
                )


toggleFeedback : Experiment -> Experiment
toggleFeedback exp =
    case exp of
        DoingMeaning (MainLoop trials state ntrial feedback) ->
            DoingMeaning (MainLoop trials state ntrial (not feedback))

        DoingTranslation (MainLoop trials state ntrial feedback) ->
            DoingTranslation (MainLoop trials state ntrial (not feedback))

        DoingScrabble (MainLoop trials state ntrial feedback) ->
            DoingScrabble (MainLoop trials state ntrial (not feedback))

        DoingSynonym (MainLoop trials state ntrial feedback) ->
            DoingSynonym (MainLoop trials state ntrial (not feedback))

        DoingSynonym (Intro trials state ntrial feedback instructions) ->
            DoingSynonym (Intro trials state ntrial (not feedback) instructions)

        _ ->
            Failure (Http.BadBody <| errorMessage "toggle feedback" "Experiment.toggleFeedback")


errorMessage : String -> String -> String
errorMessage action functioname =
    String.concat
        [ "I tried to "
        , action
        , "but I ended into an ignored case. Please report this error or update the "
        , functioname
        , " to take in account this case"
        ]


getTrialnumber : Step a b c d -> TrialNumber
getTrialnumber step =
    case step of
        MainLoop trials state trialn _ ->
            trialn

        _ ->
            9999


type Step mainTrials mainState instructions end
    = Intro mainTrials mainState Trialn IsFeedback instructions
    | Pause
    | MainLoop mainTrials mainState Trialn IsFeedback
    | End end


getFeedbackStatus : Step b c d e -> ToggleFeedback
getFeedbackStatus step =
    case step of
        MainLoop _ _ _ isfeedback ->
            isfeedback

        _ ->
            False


type alias Instructions =
    String


type alias Conclusion =
    String


type Experiment
    = NotStarted
    | Loading
    | DoingMeaning (Step (List TrialMeaning) StateMeaning Instructions Conclusion)
    | DoingTranslation (Step (List TranslationInput) TranslationOutput Instructions Conclusion)
    | DoingScrabble (Step (List ScrabbleTrial) ScrabbleState Instructions Conclusion)
    | DoingSynonym (Step (List SynonymTrial) SynonymState Instructions Conclusion)
    | Done
    | Failure Http.Error


updateState : StateType -> Experiment -> Experiment
updateState newState exp =
    case ( newState, exp ) of
        ( MeaningState newState_, DoingMeaning (MainLoop trials _ ntrial feedback) ) ->
            DoingMeaning (MainLoop trials newState_ ntrial feedback)

        ( TranslationState newState_, DoingTranslation (MainLoop trials _ ntrial feedback) ) ->
            DoingTranslation (MainLoop trials newState_ ntrial feedback)

        ( ScrabbleStateType newState_, DoingScrabble (MainLoop trials _ ntrial feedback) ) ->
            DoingScrabble (MainLoop trials newState_ ntrial feedback)

        ( SynonymStateType newState_, DoingSynonym (MainLoop trials _ ntrial feedback) ) ->
            DoingSynonym (MainLoop trials newState_ ntrial feedback)

        ( SynonymStateType newState_, DoingSynonym (Intro trials _ ntrial feedback instructions) ) ->
            DoingSynonym (Intro trials newState_ ntrial feedback instructions)

        _ ->
            Failure (Http.BadBody <| errorMessage "update the state of the experiment" "Experiment.updateState")


getState : Experiment -> StateType
getState experiment =
    case experiment of
        DoingMeaning (MainLoop trials state ntrial feedback) ->
            MeaningState state

        DoingTranslation (MainLoop trials state ntrial feedback) ->
            TranslationState state

        DoingScrabble (MainLoop trials state ntrial feedback) ->
            ScrabbleStateType state

        DoingSynonym (MainLoop trials state ntrial feedback) ->
            SynonymStateType state

        _ ->
            DummyType


type alias Trialn =
    Int


type alias IsFeedback =
    Bool



--updateState : state -> { exp | state : state } -> { exp | state : state }
--getState : { exp | state : state } -> state
--getState { state } =
--  state


type StateType
    = MeaningState StateMeaning
    | TranslationState TranslationOutput
    | ScrabbleStateType ScrabbleState
    | SynonymStateType SynonymState
    | DummyType


type TrialType
    = MeaningTrials (List TrialMeaning)
    | TranslationTrials (List TranslationInput)
    | DummyTrial ()


convert : TrialType -> List TrialMeaning
convert trialType =
    case trialType of
        MeaningTrials tr ->
            tr

        _ ->
            [ defaultTrial ]


type Answer
    = Selected Correctness
    | NotSelected


type Correctness
    = Correct
    | Incorrect


type alias ToggleFeedback =
    Bool


type alias TrialNumber =
    Int


initState : StateMeaning
initState =
    StateMeaning "DefaultTrialUID" "DefaultUserUID" ""


type UID
    = UID String


getTrialsFromServer_ : String -> (Result Http.Error a -> msg) -> Decode.Decoder a -> Cmd msg
getTrialsFromServer_ tableName callbackMsg decoder =
    Http.get
        { url =
            buildQuery
                { app = apps.spacing
                , base = "input"
                , view_ = tableName
                }
        , expect =
            Http.expectJson
                callbackMsg
                decoder
        }


defaultTrial : TrialMeaning
defaultTrial =
    { uid = "MISSING"
    , writtenWord = "MISSING"
    , definition = "MISSING"
    , option1 = "MISSING"
    , option2 = "MISSING"
    , option3 = "MISSING"
    , feedbackCorrect = "MISSING"
    , feedbackIncorrect = "MISSING"
    }



-- Translation


type alias TranslationInput =
    { uid : String
    , question : String
    , translation1 : String
    , translation2 : String
    , distractor1 : String
    , distractor2 : String
    , distractor3 : String
    , distractor4 : String
    , word : String
    }


defaultTranslationTrial : TranslationInput
defaultTranslationTrial =
    { uid = ""
    , question = "String"
    , translation1 = "String"
    , translation2 = "String"
    , distractor1 = ""
    , distractor2 = ""
    , distractor3 = ""
    , distractor4 = ""
    , word = "String"
    }


type alias TranslationOutput =
    { inputUid : String
    , userUID : String
    , userAnswer : String
    }


initTranslationState : TranslationOutput
initTranslationState =
    { inputUid = ""
    , userUID = ""
    , userAnswer = ""
    }



--MEANING


type alias TrialMeaning =
    { uid : String
    , writtenWord : String
    , definition : String
    , option1 : String
    , option2 : String
    , option3 : String
    , feedbackCorrect : String
    , feedbackIncorrect : String
    }


type alias StateMeaning =
    { inputUid : String
    , userUID : String
    , userAnswer : String
    }



-- SCRABBLE


type alias ScrabbleTrial =
    { uid : String
    , writtenWord : String
    , audioWord : Data.AudioFile
    }


type alias ScrabbleState =
    { uid : String
    , userAnswer : String
    , scrambledLetter : List KeyedItem
    }


type alias Item =
    String


type alias KeyedItem =
    ( String, Item )



--Synonym


type alias SynonymTrial =
    { uid : String
    , target : String
    , pre : String
    , stimulus : String
    , post : String
    , isTraining : Bool
    , radical : String
    }


type alias SynonymState =
    { inputUid : String
    , userUID : String
    , userAnswer : String
    }



--Spelling
