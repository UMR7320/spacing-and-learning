module ProgressBar exposing (..)

import Css
import Html.Styled as Html exposing (..)
import Html.Styled.Attributes as Html exposing (..)
import Logic
import Pretest.Version exposing (Version(..))
import Route exposing (..)
import Tuple
import View



{- This module handles the progress bar displayed at the top of the page
   during activities. It contains a lot of code duplication and should
   probably be refactored to make sure that it does not go out of sync if
   activities are re-organized.
-}


view model =
    case model.route of
        Pretest _ activity _ ->
            viewPretest activity model

        Session1 _ activity ->
            viewSession1 activity model

        AuthenticatedSession2 _ activity ->
            viewSession2 activity model

        AuthenticatedSession3 _ activity ->
            viewSession3 activity model

        Posttest _ _ session ->
            viewWordCloud session

        _ ->
            text ""


viewPretest activity model =
    case model.version of
        PreTest ->
            div
                [ class "progress-bar", attribute "style" "--count: 5" ]
                [ viewItem "General Vocabulary" YesNo model.yesno activity
                , viewItem "LexLearn verbs" VKS model.vks.task activity
                , viewItem "Reading test" SPR model.spr activity
                , viewItem "Writing test" SentenceCompletion model.sentenceCompletion activity
                , viewItem "Listening test" (isAcceptability activity) model.acceptabilityTask True
                ]

        PostTest ->
            div
                [ class "progress-bar", attribute "style" "--count: 7" ]
                [ div [] [ text "Meaning" ]
                , div [] [ text "Spelling" ]
                , div [] [ text "Context" ]
                , div [] [ text "How was it?" ]
                , viewItem "LexLearn verbs" VKS model.vks.task activity
                , viewItem "Reading test" SPR model.spr activity
                , viewItem "Listening test" (isAcceptability activity) model.acceptabilityTask True
                ]

        PostTestDiff ->
            div
                [ class "progress-bar", attribute "style" "--count: 4" ]
                [ viewItem "LexLearn verbs" VKS model.vks.task activity
                , viewItem "Reading test" SPR model.spr activity
                , viewItem "Writing test" SentenceCompletion model.sentenceCompletion activity
                , viewItem "Listening test" (isAcceptability activity) model.acceptabilityTask True
                ]

        Surprise ->
            div
                [ class "progress-bar", attribute "style" "--count: 4" ]
                [ viewItem "LexLearn verbs" VKS model.vks.task activity
                , viewItem "Reading test" SPR model.spr activity
                , viewItem "Writing test" SentenceCompletion model.sentenceCompletion activity
                , viewItem "Listening test" (isAcceptability activity) model.acceptabilityTask True
                ]

        Unknown _ ->
              text ""


viewSession1 activity model =
    div
        [ class "progress-bar", attribute "style" "--count: 5" ]
        [ viewItem "Presentation" Presentation model.presentation activity
        , viewItem "Meaning" Meaning model.meaning activity
        , viewItem "Spelling" SpellingLevel1 model.spellingLvl1 activity
        , viewItem "Context" CU1 model.cu1 activity
        , div [ classList [ ( "active", False ) ] ] [ text "How was it?" ]
        ]


viewSession2 activity model =
    div
        [ class "progress-bar", attribute "style" "--count: 4" ]
        [ viewItem "Meaning" Translation model.translationTask activity
        , viewItem "Spelling" Spelling model.scrabbleTask activity
        , viewItem "Context" CU model.cuLvl2 activity
        , div [] [ text "How was it?" ]
        ]


viewSession3 activity model =
    div
        [ class "progress-bar", attribute "style" "--count: 7" ]
        [ viewItem "Meaning" Synonym model.synonymTask activity
        , viewItem "Spelling" Spelling3 model.spelling3 activity
        , viewItem "Context" CU3 model.cu3 activity
        , div [] [ text "How was it?" ]
        , div [] [ text "LexLearn verbs" ]
        , div [] [ text "Reading test" ]
        , div [] [ text "Listening test" ]
        ]


viewItem name activity task currentActivity =
    div
        [ classList [ ( "active", currentActivity == activity ) ] ]
        [ div []
            [ text name
            , case task of
                Logic.Running _ data ->
                    viewActivityProgressBar data

                _ ->
                    text ""
            ]
        ]


viewWordCloud session =
    case session of
        Just "S1" ->
            div
                [ class "progress-bar", attribute "style" "--count: 5" ]
                [ div [] [ text "Presentation" ]
                , div [] [ text "Meaning" ]
                , div [] [ text "Spelling" ]
                , div [] [ text "Context" ]
                , div [ class "active" ] [ text "How was it?" ]
                ]

        Just "S2" ->
            div
                [ class "progress-bar", attribute "style" "--count: 4" ]
                [ div [] [ text "Meaning" ]
                , div [] [ text "Spelling" ]
                , div [] [ text "Context" ]
                , div [ class "active" ] [ text "How was it?" ]
                ]

        Just "S3" ->
            div
                [ class "progress-bar", attribute "style" "--count: 7" ]
                [ div [] [ text "Meaning" ]
                , div [] [ text "Spelling" ]
                , div [] [ text "Context" ]
                , div [ class "active" ] [ text "How was it?" ]
                , div [] [ text "LexLearn verbs" ]
                , div [] [ text "Reading test" ]
                , div [] [ text "Listening test" ]
                ]

        _ ->
            text ""


isAcceptability activity =
    case activity of
        Acceptability _ ->
            True

        _ ->
            False



-- ACTIVITY PROGRESS BAR


viewActivityProgressBar { history, trainingTrials, mainTrials } =
    let
        l =
            List.length >> toFloat

        pct_ =
            l history / (l history + l trainingTrials + l mainTrials) * 100
    in
    div
        [ class "activity-progress" ]
        [ div
            [ class "activity-progress--inner"
            , Html.css [ Css.width (Css.pct pct_) ]
            ]
            []
        ]
