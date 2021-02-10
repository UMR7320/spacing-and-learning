module Route exposing
    ( Route(..)
    , fromUrl
    )

import Url
import Url.Parser as Parser exposing ((</>), Parser, int, map, oneOf, s, top)


type Route
    = ExperimentStart
    | Home
    | Meaning
    | Translation
    | Scrabble
    | CloudWords
    | Acceptability
    | NotFound
    | Synonym
    | LogIn
    | SpellingLevel1
    | CULevel2
    | CU1


type Level
    = Level1
    | Level2
    | Level3


parser : Parser (Route -> a) a
parser =
    oneOf
        [ map ExperimentStart top
        , map Home (s "index.html")
        , map Scrabble (s "scrabble")
        , map ExperimentStart (s "start")
        , map Meaning (s "meaning")
        , map Translation (s "translation")
        , map CloudWords (s "cloudwords")
        , map Synonym (s "synonym")
        , map Acceptability (s "acceptability")
        , map LogIn (s "login")
        , map SpellingLevel1 (s "spelling")
        , map CULevel2 (s "cu-lvl2")
        , map CU1 (s "cu1")

        --  Add more routes like this:
        --  , map Comment (s "user" </> string </> s "comment" </> int)
        --  , map BlogQuery (s "blog" <?> Query.string "q")
        --  Learn more: https://guide.elm-lang.org/webapps/url_parsing.html
        ]


fromUrl : Url.Url -> Route
fromUrl url =
    Parser.parse parser url
        |> Maybe.withDefault NotFound
