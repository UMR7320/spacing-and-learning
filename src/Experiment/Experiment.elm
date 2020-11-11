module Experiment.Experiment exposing
    ( Block(..)
    ,  Experiment(..)
       --, MeaningInput

    , Step(..)
    , TranslationInput
    , nextTrial
    , toggleFeedback
    )

--import Experiment.Meaning exposing (MeaningInput, MeaningOutput)

import Http


type Block input output
    = Meaning (List input) output
    | Translation
    | Synonym
    | ClosedChoiceSpelling
    | ScrabbleSpelling
    | FreeWritingSpelling
    | ClosedChoiceTextCompletion
    | ClosedChoiceTextAndAudioUnderstanding
    | FreeWritingTextCompletion


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


type alias ToggleFeedback =
    Bool


type alias TrialNumber =
    Int


type Experiment a b
    = NotAsked
    | Loading
    | Ready ( Block a b, Step )
    | Failure Http.Error


type UID
    = UID String



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
