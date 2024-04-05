module ProgressBar exposing (..)

import Activity
import Css
import Html.Styled as Html exposing (..)
import Html.Styled.Attributes as Html exposing (..)
import Pretest.Version exposing (Version(..))
import Route exposing (..)



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

        Session2 _ activity ->
            viewSession2 activity model

        Session3 _ activity ->
            viewSession3 activity model

        _ ->
            div [] [ text "" ]


viewPretest activity model =
    case model.version of
        PreTest ->
            div
                [ class "progress-bar", attribute "style" "--count: 2" ]
                [ viewItem "Premier Quizz" (isYesNo activity) model.yesNo True
                , viewItem "Deuxième Quizz" (isVKS activity) model.vks True
                ]

        PostTest ->
            div
                [ class "progress-bar", attribute "style" "--count: 5" ]
                [ div [] [ text "Meaning" ]
                , div [] [ text "Spelling" ]
                , div [] [ text "Context" ]
                , div [] [ text "How was it?" ]
                , viewItem "Quizz" (isVKS activity) model.vks True
                -- , viewItem "Reading test" SPR model.spr activity
                -- , viewItem "Listening test" (isAcceptability activity) model.acceptability True
                ]

        PostTestDiff ->
            div
                [ class "progress-bar", attribute "style" "--count: 1" ]
                [ viewItem "LexLearn verbs" (isVKS activity) model.vks True
                ]

        Surprise ->
            div
                [ class "progress-bar", attribute "style" "--count: 4" ]
                [ viewItem "LexLearn verbs" (isVKS activity) model.vks True
                , viewItem "Reading test" SPR model.spr activity
                , viewItem "Writing test" SentenceCompletion model.sentenceCompletion activity
                , viewItem "Listening test" (isAcceptability activity) model.acceptability True
                ]

        Unknown _ ->
            text ""


viewSession1 activity model =
    div
        [ class "progress-bar", attribute "style" "--count: 5" ]
        [ viewItem "Presentation" Presentation model.presentation activity
        , viewItem "Meaning" Meaning1 model.meaning1 activity
        , viewItem "Spelling" Spelling1 model.spelling1 activity
        , viewItem "Context" Context1 model.context1 activity
        , div [ classList [ ( "active", activity == WordCloud1 ) ] ] [ text "How was it?" ]
        ]


viewSession2 activity model =
    div
        [ class "progress-bar", attribute "style" "--count: 4" ]
        [ viewItem "Meaning" Meaning2 model.meaning2 activity
        , viewItem "Spelling" Spelling2 model.spelling2 activity
        , viewItem "Context" Context2 model.context2 activity
        , div [ classList [ ( "active", activity == WordCloud2 ) ] ] [ text "How was it?" ]
        ]


viewSession3 activity model =
    div
        [ class "progress-bar", attribute "style" "--count: 5" ]
        [ viewItem "Meaning" Meaning3 model.meaning3 activity
        , viewItem "Spelling" Spelling3 model.spelling3 activity
        , viewItem "Context" Context3 model.context3 activity
        , div [ classList [ ( "active", activity == WordCloud3 ) ] ] [ text "How was it?" ]
        , div [] [ text "Quizz" ]
        ]


viewItem name activity task currentActivity =
    div
        [ classList [ ( "active", currentActivity == activity ) ] ]
        [ div []
            [ text name
            , case task of
                Activity.Running _ data ->
                    viewActivityProgressBar data

                _ ->
                    text ""
            ]
        ]


isYesNo activity =
    case activity of
        YesNo _ ->
            True

        _ ->
            False


isVKS activity =
    case activity of
        VKS _ ->
            True

        _ ->
            False


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
