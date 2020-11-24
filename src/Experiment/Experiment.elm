module Experiment.Experiment exposing (..)

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
                DoingMeaning End

            else
                DoingMeaning (MainLoop trials state (ntrial + 1) feedback)

        DoingTranslation (MainLoop trials state ntrial feedback) ->
            if ntrial >= List.length trials - 1 then
                DoingTranslation End

            else
                DoingTranslation (MainLoop trials state (ntrial + 1) feedback)

        DoingScrabble (MainLoop trials state ntrial feedback) ->
            if ntrial >= List.length trials - 1 then
                DoingScrabble End

            else
                DoingScrabble (MainLoop trials state (ntrial + 1) feedback)

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


getTrialnumber : Step a b c d e f -> TrialNumber
getTrialnumber step =
    case step of
        MainLoop trials state trialn _ ->
            trialn

        _ ->
            9999


type Step mainTrainingTrials mainTrainingState mainTrials mainState instructions end
    = Intro instructions
    | Training mainTrainingTrials mainTrainingState Trialn IsFeedback
    | Pause
    | MainLoop mainTrials mainState Trialn IsFeedback
    | End


getFeedbackStatus : Step b c d e f g -> ToggleFeedback
getFeedbackStatus step =
    case step of
        MainLoop _ _ _ isfeedback ->
            isfeedback

        _ ->
            False


type Experiment
    = NotStarted
    | Loading
    | DoingMeaning (Step (List ()) () (List TrialMeaning) StateMeaning String String)
    | DoingTranslation (Step (List ()) () (List TranslationInput) TranslationOutput String String)
    | DoingScrabble (Step (List ()) () (List ScrabbleTrial) ScrabbleState String String)
    | Done
    | Failure Http.Error


updateState : StateType -> Experiment -> Experiment
updateState newState exp =
    case ( newState, exp ) of
        ( MeaningState newState_, DoingMeaning (MainLoop trials prevstate ntrial feedback) ) ->
            DoingMeaning (MainLoop trials newState_ ntrial feedback)

        ( TranslationState newState_, DoingTranslation (MainLoop trials prevstate ntrial feedback) ) ->
            DoingTranslation (MainLoop trials newState_ ntrial feedback)

        ( ScrabbleStateType newState_, DoingScrabble (MainLoop trials prevstate ntrial feedback) ) ->
            DoingScrabble (MainLoop trials newState_ ntrial feedback)

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
    , question = "MISSING"
    , option1 = "MISSING"
    , option2 = "MISSING"
    , option3 = "MISSING"
    , option4 = "DEFAULT"
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
    , question : String
    , option1 : String
    , option2 : String
    , option3 : String
    , option4 : String
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
