module Session1.Top exposing (..)

import Dict
import ExperimentInfo
import Html.Styled exposing (a, div, h1, h2, p, text)
import Html.Styled.Attributes exposing (class, href)
import Session3.Synonym exposing (Msg(..))


view infos =
    let
        toCard =
            \info ->
                div [ Html.Styled.Attributes.href info.url, class " my-1 px-1 w-full md:w-1/2 lg:my-4 lg:px-4 lg:w-13 transform scale-100 transition duration-150 hover:scale-105 cursor-pointer" ]
                    [ Html.Styled.article [ class "overflow-hidden rounded-lg shadow-md hover:shadow-xl" ]
                        [ a [ href info.url ] [ Html.Styled.img [ class "block h-auto w-full", Html.Styled.Attributes.src "https://picsum.photos/600/400/?random" ] [] ]
                        , Html.Styled.header [ class "flex items-center justify-between leading-tight p-2 md:p4" ]
                            [ h1 [] [ text info.name ]
                            ]
                        , p [ class "px-2 overflow-ellipsis" ] [ text info.description ]
                        , Html.Styled.footer
                            [ class "flex items-center justify-between leading-none p-2 md:p-4" ]
                            [ div [ class "bg-green-500 rounded-lg p-2 text-white" ] [ text (ExperimentInfo.typeToString info.type_) ]
                            , div [ class "bg-blue-500 rounded-lg p-2 text-white" ] [ text (ExperimentInfo.sessionToString info.session) ]
                            ]
                        ]
                    ]

        viewCard with =
            infos
                |> Dict.toList
                |> List.map Tuple.second
                |> List.filter with
                |> List.map toCard
                |> div [ class "flex flex-wrap -mx-1 px-4 md:px-12" ]
    in
    [ div [ class "flex flex-col items-center justify-center w-full max-w-2-xl" ]
        [ h1 [] [ text "Lex Learn 👩\u{200D}🎓️" ]
        , p
            [ class "max-w-2xl text-xl text-center mb-8" ]
            [ text "Lorem ipsum dolor sit amet, consectetur adipiscing elit, sed do eiusmod tempor incididunt ut labore et dolore magna aliqua.\n Sapien et ligula ullamcorper malesuada proin libero nunc consequat. Sed sed risus pretium quam vulputate dignissim. Aliquam sem fringilla ut morbi tincidunt augue interdum velit euismod. Ultrices tincidunt arcu non sodales neque."
            ]
        , h2 [] [ text "Session 1" ]
        , viewCard (\info -> info.session == ExperimentInfo.Session1)
        ]
    ]
