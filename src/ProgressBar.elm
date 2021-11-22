module ProgressBar exposing (..)

import Css
import Html.Styled as Html exposing (..)
import Html.Styled.Attributes as Html exposing (..)
import Logic
import Route exposing (..)
import Tuple
import View



{- This module handles the progress bar displayed at the top of the page
   during activities. It contains a lot of code duplication and should
   probably be refactored to make sure that it does not go out of sync if
   activities are re-organized.
-}


type Version
    = PreTest
    | PostTest
    | PostTestDiff
    | SurprisePostTest


view model =
    case model.route of
        Pretest _ activity _ ->
            viewPretest activity model.version

        Session1 _ activity ->
            viewSession1 activity model

        AuthenticatedSession2 _ activity ->
            viewSession2 activity

        AuthenticatedSession3 _ activity ->
            viewSession3 activity

        Posttest _ _ session ->
            viewWordCloud session

        _ ->
            text ""


viewPretest activity version =
    case maybeStringToVersion version of
        PreTest ->
            div
                [ class "progress-bar", attribute "style" "--count: 5" ]
                [ div [ classList [ ( "active", activity == YesNo ) ] ] [ text "Yes/No" ]
                , div [ classList [ ( "active", activity == VKS ) ] ] [ text "VKS" ]
                , div [ classList [ ( "active", activity == SPR ) ] ] [ text "SPR" ]
                , div [ classList [ ( "active", activity == SentenceCompletion ) ] ] [ text "Sentence Completion" ]
                , div [ classList [ ( "active", isAcceptability activity ) ] ] [ text "Acceptability" ]
                ]

        PostTest ->
            div
                [ class "progress-bar", attribute "style" "--count: 7" ]
                [ div [] [ text "Meaning" ]
                , div [] [ text "Form" ]
                , div [] [ text "Use" ]
                , div [] [ text "WordCloud" ]
                , div [ classList [ ( "active", activity == VKS ) ] ] [ text "VKS" ]
                , div [ classList [ ( "active", activity == SPR ) ] ] [ text "SPR" ]
                , div [ classList [ ( "active", isAcceptability activity ) ] ] [ text "Acceptability" ]
                ]

        PostTestDiff ->
            div
                [ class "progress-bar", attribute "style" "--count: 4" ]
                [ div [ classList [ ( "active", activity == VKS ) ] ] [ text "VKS" ]
                , div [ classList [ ( "active", activity == SPR ) ] ] [ text "SPR" ]
                , div [ classList [ ( "active", activity == SentenceCompletion ) ] ] [ text "Sentence Completion" ]
                , div [ classList [ ( "active", isAcceptability activity ) ] ] [ text "Acceptability" ]
                ]

        SurprisePostTest ->
            div
                [ class "progress-bar", attribute "style" "--count: 4" ]
                [ div [ classList [ ( "active", activity == VKS ) ] ] [ text "VKS" ]
                , div [ classList [ ( "active", activity == SPR ) ] ] [ text "SPR" ]
                , div [ classList [ ( "active", activity == SentenceCompletion ) ] ] [ text "Sentence Completion" ]
                , div [ classList [ ( "active", isAcceptability activity ) ] ] [ text "Acceptability" ]
                ]


viewSession1 activity model =
    div
        [ class "progress-bar", attribute "style" "--count: 5" ]
        [ viewItem "Presentation" Presentation model.presentation activity
        , viewItem "Meaning" Meaning model.meaning activity
        , viewItem "Form" SpellingLevel1 model.spellingLvl1 activity
        , viewItem "Context" CU1 model.cu1 activity
        , div [ classList [ ( "active", False ) ] ] [ text "WordCloud" ]
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


viewSession2 activity =
    div
        [ class "progress-bar", attribute "style" "--count: 4" ]
        [ div [ classList [ ( "active", activity == Translation ) ] ] [ text "Meaning" ]
        , div [ classList [ ( "active", activity == Spelling ) ] ] [ text "Form" ]
        , div [ classList [ ( "active", activity == CU ) ] ] [ text "Use" ]
        , div [ classList [ ( "active", False ) ] ] [ text "WordCloud" ]
        ]


viewSession3 activity =
    div
        [ class "progress-bar", attribute "style" "--count: 7" ]
        [ div [ classList [ ( "active", activity == Synonym ) ] ] [ text "Meaning" ]
        , div [ classList [ ( "active", activity == Spelling3 ) ] ] [ text "Form" ]
        , div [ classList [ ( "active", activity == CU3 ) ] ] [ text "Use" ]
        , div [ classList [ ( "active", False ) ] ] [ text "WordCloud" ]
        , div [] [ text "VKS" ]
        , div [] [ text "SPR" ]
        , div [] [ text "Acceptability" ]
        ]


viewWordCloud session =
    if session == Just "S3" then
        div
            [ class "progress-bar", attribute "style" "--count: 7" ]
            [ div [] [ text "Meaning" ]
            , div [] [ text "Form" ]
            , div [] [ text "Use" ]
            , div [ class "active" ] [ text "WordCloud" ]
            , div [] [ text "VKS" ]
            , div [] [ text "SPR" ]
            , div [] [ text "Acceptability" ]
            ]

    else
        text ""


maybeStringToVersion : Maybe String -> Version
maybeStringToVersion maybeVersion =
    Maybe.map stringToVersion maybeVersion
        |> Maybe.withDefault PreTest


stringToVersion : String -> Version
stringToVersion version =
    case version of
        "post" ->
            PostTest

        "post-diff" ->
            PostTestDiff

        "surprise" ->
            SurprisePostTest

        _ ->
            PreTest


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
