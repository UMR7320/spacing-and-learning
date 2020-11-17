module Experiment.Experiment exposing
    ( Answer(..)
    , Block(..)
    , Correctness(..)
    ,  Experiment(..)
       --, MeaningInput

    , Step(..)
    , TranslationInput
    , apps
    , buildQuery
    , getState
    , getTrialsFromServer_
    , nextTrial
    , resetAnswerState
    , toggleFeedback
    )

--import Experiment.Meaning exposing (MeaningInput, MeaningOutput)

import Array
import Http
import Json.Decode as Decode
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


nextTrial : Experiment a b -> Experiment a b
nextTrial experiment =
    case experiment of
        Ready ( (Meaning trials state) as block, MainLoop trialNumber feedback ) ->
            if trialNumber >= List.length trials - 1 then
                Ready ( block, End )

            else
                Ready ( block, MainLoop (trialNumber + 1) feedback )

        _ ->
            Failure
                (Http.BadBody "I tried to go to next trial but ended into an unexpected case. Please report this error message if you see it.")


toggleFeedback : Experiment a b -> Experiment a b
toggleFeedback experiment =
    case experiment of
        Ready ( (Meaning trials state) as block, MainLoop trialNumber feedback ) ->
            Ready ( block, MainLoop trialNumber (not feedback) )

        Ready ( currentBlock, End ) ->
            Ready ( currentBlock, End )

        _ ->
            Failure
                (Http.BadBody "I tried to toggle feedback but ended into an unexpected case. Please report this error message if you see it.")


getTrial_ : Experiment trial state -> Maybe trial
getTrial_ exp =
    case exp of
        Ready ( Meaning trials state, MainLoop ntrial _ ) ->
            Array.get ntrial (Array.fromList trials)

        _ ->
            Nothing


resetAnswerState : Experiment trial { b | userAnswer : String } -> Experiment trial { b | userAnswer : String }
resetAnswerState experiment =
    case experiment of
        Ready ( (Meaning trials state) as block, MainLoop trialNumber feedback ) ->
            Ready ( Meaning trials { state | userAnswer = "" }, MainLoop trialNumber feedback )

        Ready ( currentBlock, End ) ->
            Ready ( currentBlock, End )

        _ ->
            Failure
                (Http.BadBody "I tried to reset answer but ended into an unexpected case. Please report this error message if you see it.")


toBlockAndStep : ( Block a b, Step ) -> Experiment a b -> Experiment a b
toBlockAndStep ( newBlock, newStep ) experiment =
    case experiment of
        Ready ( prevBlock, prevStep ) ->
            Ready ( newBlock, newStep )

        _ ->
            Failure (Http.BadBody "I tried to change the current block or step and I ended into an unexpected case. Please report the error if you see it.")


type Step
    = Intro
    | Training
    | Pause
    | MainLoop TrialNumber ToggleFeedback
    | End


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


type Experiment trial state
    = NotAsked
    | Loading
    | Ready ( Block trial state, Step )
    | Failure Http.Error


type Block trial state
    = Meaning (List trial) state
    | Translation
    | Synonym
    | ClosedChoiceSpelling
    | ScrabbleSpelling
    | FreeWritingSpelling
    | ClosedChoiceTextCompletion
    | ClosedChoiceTextAndAudioUnderstanding
    | FreeWritingTextCompletion


getState : Experiment trial state -> Maybe state
getState exp =
    case exp of
        Ready ( Meaning trials state, step ) ->
            Just state

        _ ->
            Nothing


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



-- Translation


type alias TranslationInput =
    { uid : String
    , question : String
    , option1 : String
    , option2 : String
    , word : String
    }


type alias TranslationOutput =
    { inputUid : UID
    , userUID : UID
    , userAnswer : String
    }
