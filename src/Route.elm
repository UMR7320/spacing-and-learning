module Route exposing
    ( AcceptabilityRoute(..)
    , PosttestTask(..)
    , PretestTask(..)
    , Route(..)
    , Session1Task(..)
    , Session2Task(..)
    , Session3Task(..)
    , fromUrl
    )

import Url
import Url.Parser as Parser
    exposing
        ( (</>)
        , Parser
        , map
        , oneOf
        , s
        , string
        , top
        )


type Route
    = Home
    | NotFound
    | Pretest PretestTask
    | Session1 UserId Session1Task
    | AuthenticatedSession2 UserId Session2Task
    | AuthenticatedSession3 UserId Session3Task
    | Pilote UserId AcceptabilityRoute
    | Posttest PosttestTask


type alias UserId =
    String


type Session1Task
    = Meaning
    | SpellingLevel1
    | Presentation
    | CU1
    | TopSession1


type Session2Task
    = Translation
    | Spelling
    | CU


type PretestTask
    = YN
    | GeneralInfos
    | EmailSent
    | SPR
    | SentenceCompletion
    | VKS


type AcceptabilityRoute
    = AcceptabilityInstructions
    | AcceptabilityStart
    | AcceptabilityEnd



--| Acceptability


type PosttestTask
    = CloudWords


type Session3Task
    = CU3
    | Spelling3
    | Synonym


parser : Parser (Route -> a) a
parser =
    oneOf
        [ map Home top
        , map Pretest
            (s "pretest"
                </> oneOf
                        [ map SPR (s "spr")
                        , map YN (s "yesno-task")
                        , map GeneralInfos (s "informations")
                        , map EmailSent (s "email-sent")
                        , map SentenceCompletion (s "sentence-completion")
                        , map VKS (s "vks")
                        ]
            )
        , map Pilote
            (s "user"
                </> string
                </> s "pilote"
                </> s "acceptability"
                </> oneOf
                        [ map AcceptabilityInstructions (s "instructions")
                        , map AcceptabilityStart (s "start")
                        , map AcceptabilityEnd (s "end")
                        ]
            )
        , map Pretest
            (s "posttest"
                </> oneOf
                        [ map YN (s "cloud-words") ]
            )
        , map Session1
            (s "user"
                </> string
                </> s "session1"
                </> oneOf
                        [ map Meaning (s "meaning")
                        , map SpellingLevel1 (s "spelling")
                        , map Presentation (s "presentation")
                        , map CU1 (s "context-understanding")
                        , map TopSession1 top
                        ]
            )
        , map AuthenticatedSession2
            (s "user"
                </> string
                </> s "session2"
                </> oneOf
                        [ map Spelling (s "spelling")
                        , map Translation (s "translation")
                        , map CU (s "context-understanding")
                        ]
            )
        , map AuthenticatedSession3
            (s "user"
                </> string
                </> s "session3"
                </> oneOf
                        [ map CU3 (s "context-understanding")
                        , map Spelling3 (s "spelling")
                        , map Synonym (s "synonym")
                        ]
            )

        --  Add more routes like this:
        --  , map Comment (s "user" </> string </> s "comment" </> int)
        --  , map BlogQuery (s "blog" <?> Query.string "q")
        --  Learn more: https://guide.elm-lang.org/webapps/url_parsing.html
        ]



--translationPath : Parser (String -> a) a
--userAndTask : a -> b -> c -> d -> e -> f -> g -> h -> i -> Url.Url -> Route
--spacing-and-learning.netlify.app/user/userId/task/scrabble


fromUrl : Url.Url -> Route
fromUrl url =
    Parser.parse parser url
        |> Maybe.withDefault NotFound
